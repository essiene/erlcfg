// 
// Copyright (c) 2014, Serge Aleynikov
// All rights reserved.
// 
// Redistribution and use in source and binary forms, 
// with or without modification, are permitted 
// provided that the following conditions are met:
//
//    * Redistributions of source code must retain the 
//      above copyright notice, this list of conditions 
//      and the following disclaimer.
//    * Redistributions in binary form must reproduce 
//      the above copyright notice, this list of 
//      conditions and the following disclaimer in the 
//      documentation and/or other materials provided with 
//      the distribution.
//    * Neither the name "JsonEvents" nor the names of its 
//      contributors may be used to endorse or promote 
//      products derived from this software without 
//      specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND 
// CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED 
// WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR 
// PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT 
// HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, 
// INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES 
// (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE 
// GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR 
// BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF 
// LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT 
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF 
// THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY 
// OF SUCH DAMAGE.
// 

#if defined(_MSC_VER) || defined(_WIN32) || defined(__CYGWIN32__)
#pragma warning(disable : 26451)
#pragma warning(disable : 4530)
#pragma warning(disable : 4577)
#pragma warning(disable : 4625)
#pragma warning(disable : 4626)
#pragma warning(disable : 4668)
#pragma warning(disable : 4710)
#pragma warning(disable : 4774)
#pragma warning(disable : 4820)
#pragma warning(disable : 5026)
#pragma warning(disable : 5027)
#pragma warning(disable : 5045)
#include <Windows.h>
#endif

#include <erl_nif.h>
#include <string.h>
#include <ctime>
#include <string>
#include <tuple>
#include <regex>
#include <iterator>

static ERL_NIF_TERM ATOM_OK;
static ERL_NIF_TERM ATOM_ERROR;
static ERL_NIF_TERM ATOM_UTC;
static ERL_NIF_TERM ATOM_TOO_LONG;
static ERL_NIF_TERM ATOM_ENOMEM;
static ERL_NIF_TERM ATOM_NOT_IMPL;

static int          on_load      (ErlNifEnv*, void**,         ERL_NIF_TERM);
static int          on_upgrade   (ErlNifEnv*, void**, void**, ERL_NIF_TERM);
static ERL_NIF_TERM strftime_nif (ErlNifEnv*, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM strptime_nif (ErlNifEnv*, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM pathftime_nif(ErlNifEnv*, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM strenv_nif   (ErlNifEnv*, int argc, const ERL_NIF_TERM argv[]);

static std::pair<int, bool> strftime_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM* argv, char* res, size_t sz);

extern "C" {

    static ErlNifFunc nif_funcs[] =
    {
        {"strftime", 3, strftime_nif},
        {"strptime", 2, strptime_nif},
        {"pathftime",3, pathftime_nif},
        {"strenv",   1, strenv_nif},
    };

    ERL_NIF_INIT(erlcfg_util, nif_funcs, &on_load, NULL, &on_upgrade, NULL);
};

static int on_load(ErlNifEnv* env, void** /*priv_data*/, ERL_NIF_TERM /*load_info*/)
{
    ATOM_OK       = enif_make_atom(env, "ok");
    ATOM_ERROR    = enif_make_atom(env, "error");
    ATOM_UTC      = enif_make_atom(env, "utc");
    ATOM_TOO_LONG = enif_make_atom(env, "too_long");
    ATOM_ENOMEM   = enif_make_atom(env, "enomem");
    ATOM_NOT_IMPL = enif_make_atom(env, "not_implemented");
    return 0;
}

static int on_upgrade(ErlNifEnv* env, void** priv_data, void**, ERL_NIF_TERM load_info)
{
    return on_load(env, priv_data, load_info);
}

std::string my_getenv(const char* var)
{
  #if defined(_WIN32) || defined(_WIN64)
    size_t sz;
    char buf[256];
    if (getenv_s(&sz, buf, sizeof(buf) - 1, var) == 0)
      return std::string(buf, sz);
  #else
    const char* s = getenv(var);
    if (s) return s;
  #endif
    return std::string();
}

static std::string normalize(std::string& s) {
#if defined(_MSC_VER) || defined(_WIN32) || defined(__CYGWIN32__)
  replace(s.begin(), s.end(), '\\', '/');
#endif
  if (!s.empty() && s.back() == '/')
    s = s.erase(s.length() - 1);
  return s;
}

static std::string home() {
    auto env = my_getenv("HOME");
    #if defined(_MSC_VER) || defined(_WIN32) || defined(__CYGWIN32__)
    if (!env.empty())
      return normalize(env);
    env = my_getenv("USERPROFILE");
    if (!env.empty())
        return normalize(env);
    auto c = my_getenv("HOMEDRIVE");
    std::string home = my_getenv("HOMEPATH");
    return (c.empty() || home.empty()) ? std::string() : c + normalize(home);
    #else
    return env;
    #endif
}

std::string dirname(const std::string& file) {
  auto found = file.find_last_of("/");
  return found == std::string::npos ? file : file.substr(0, found);
}

static const std::string cs_home("HOME");

// Update the input string.
bool replace_env_vars(std::string& text) {
  try {
    const std::regex env("(?:(?:\\$\\{([^\\$\\}]+)\\})|(?:\\$([A-Za-z][^\\s$]+)))");
    std::smatch m;
    while (std::regex_search(text, m, env)) {
      auto beg = std::sregex_iterator(text.begin(), text.end(), env);
      auto end = std::sregex_iterator();

      std::string s;
      unsigned p = 0, l = 0;

      //for (auto i = beg; i != end && s.empty(); ++i) {
      for (auto index = 1u; index < m.size(); ++index) {
        //auto m = *i;
        //for (auto index = 1u; index < m.size(); ++index) {
          if (m[index].matched && !m[index].str().empty()) {
            p = unsigned(m.position(0));
            l = unsigned(m.length(0));
            s = m[index].str();
            break;
          }
        //}
      }
      if (s.empty())
        return true;
      std::string var;
      if (s == cs_home)
        var = home();
      else
        var = my_getenv(s.c_str());

      text.replace(p, l, var);
    }
    return true;
  }
  catch (std::exception& e) {
    fprintf(stderr, "Exception: %s\r\n", e.what());
    return false;
  }
}

char* maybe_copy_bin(ErlNifBinary& bin, char* buf, size_t sz)
{
    if (bin.data[bin.size] == '\0')
        return (char*)bin.data;

    size_t n = bin.size < sz ? bin.size : sz-1;
  #if defined(_WIN32) || defined(_WIN64)
    strcpy_s(buf, n, (char*)bin.data);
  #else
    strncpy(buf, (char*)bin.data, n);
  #endif
    buf[n] = '\0';
    return buf;
}

static ERL_NIF_TERM str_to_term(ErlNifEnv* env, bool is_bin, const char* str, size_t sz)
{
    if (sz <= 0)
        return enif_make_badarg(env);

    // Output is a binary
    if (is_bin) {
        ERL_NIF_TERM ret_bin;
        char* p = (char*)enif_make_new_binary(env, sz, &ret_bin);
        if (!p) 
            return enif_make_tuple2(env, ATOM_ERROR, ATOM_ENOMEM);
        memcpy(p, str, sz);
        return ret_bin;
    }

    // Output is a string
    return enif_make_string_len(env, str, sz, ERL_NIF_LATIN1);
}

// Same functionality as strftime(3)
//  (Format :: string() | binary(), Now :: {MS, S, Ms}, utc | local) ->
//      string() | binary().
static ERL_NIF_TERM strftime_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    char res[1024];

    auto r = strftime_impl(env, argc, argv, res, sizeof(res));

    return str_to_term(env, r.second, res, size_t(r.first));
}

// Same functionality as strptime(3)
//  (Format :: string() | binary(), Now :: {MS, S, Ms}, utc | local) ->
//      {DateTime :: {Date::{Y,M,D}, Time::{H,M,S}}, ProcessedLen::integer()}
static ERL_NIF_TERM strptime_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
#if defined(_MSC_VER) || defined(_WIN32) || defined(__CYGWIN32__)
    (int*)&argc; // Remove compiler warning
    (void*)argv; // Remove compiler warning
    return enif_make_tuple2(env, ATOM_ERROR, ATOM_NOT_IMPL);
#else
    char         buf[512], fmt[512];
    const char*  pbuf, *pfmt;
    ErlNifBinary sbin,  fbin;

    if (argc != 2)
        return enif_make_badarg(env);

    bool is_sbin = enif_is_binary(env, argv[0]);
    bool is_fbin = enif_is_binary(env, argv[1]);

    if (is_sbin) {
        if (!enif_inspect_binary(env, argv[0], &sbin))
            return enif_make_badarg(env);
        pbuf = maybe_copy_bin(sbin, buf, sizeof(buf));
    } else if (enif_get_string(env, argv[0], buf, sizeof(buf), ERL_NIF_LATIN1) > 0)
        pbuf = buf;
    else
        return enif_make_badarg(env);

    if (is_fbin) {
        if (!enif_inspect_binary(env, argv[1], &fbin))
            return enif_make_badarg(env);
        pfmt = maybe_copy_bin(fbin, fmt, sizeof(fmt));
    } else if (enif_get_string(env, argv[1], fmt, sizeof(fmt), ERL_NIF_LATIN1) > 0)
        pfmt = fmt;
    else
        return enif_make_badarg(env);

    // i array contains result of erlang:now()
    struct tm tm;

    char* p = strptime(pbuf, pfmt, &tm);

    if (!p)
        return enif_make_badarg(env);

    return enif_make_tuple2
        (env, enif_make_tuple2
                (env, enif_make_tuple3
                        (env
                        ,enif_make_int(env,tm.tm_year+1900)
                        ,enif_make_int(env,tm.tm_mon+1)
                        ,enif_make_int(env,tm.tm_mday)),
                      enif_make_tuple3
                        (env
                        ,enif_make_int(env,tm.tm_hour)
                        ,enif_make_int(env,tm.tm_min)
                        ,enif_make_int(env,tm.tm_sec))),
              enif_make_int(env, p - pbuf));
#endif
}

// Same functionality as strftime(3) plus environment variable substitution
//  (Format :: string() | binary(), Now :: {MS, S, Ms}, utc | local) ->
//      string() | binary().
static ERL_NIF_TERM pathftime_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    char res[1024];

    int  n;
    bool is_bin;
    std::tie(n, is_bin) = strftime_impl(env, argc, argv, res, sizeof(res));

    if (n <= 0)
        return enif_make_badarg(env);

    std::string s(res, (size_t)n);

    if (res[0] == '~') {
        auto h = home();
        if (s.length() > 1 && s[1] != '/' && h.length() > 0 && h.back() != '/')
          h.append("/");
        s.replace(s.begin(), s.begin() + 1, h);
    }
    replace_env_vars(s);

    return str_to_term(env, is_bin, s.c_str(), s.size());
}

// Replace environment variables in a string
//      (string() | binary()) -> string() | binary().
static ERL_NIF_TERM strenv_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary bin;
    std::string  s;

    if (argc != 1)
        return enif_make_badarg(env);

    bool is_bin = (bool)enif_is_binary(env, argv[0]);

    if (is_bin) {
        if (!enif_inspect_binary(env, argv[0], &bin))
            return enif_make_badarg(env);
        s.assign((const char*)bin.data, bin.size);
    } else {
      char buf[1024];
        int n = enif_get_string(env, argv[0], buf, sizeof(buf), ERL_NIF_LATIN1);
        if (n <= 0)
            return enif_make_badarg(env);
        s.assign(buf, size_t(n)-1); // Less trailing zero
    }

    return replace_env_vars(s)
         ? str_to_term(env, is_bin, s.c_str(), s.size())
         : enif_make_badarg(env);
}

// Common functionality of strftime(3) callable from other functions
//  (Format :: string() | binary(), Now :: {MS, S, Ms}, utc | local) ->
//      number of bytes written to res.
static std::pair<int, bool> 
strftime_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM* argv, char* res, size_t sz)
{
    const ERL_NIF_TERM* time_tup;
    long         i0, i1;
    int          arity;
    char         buf[512];
    const char*  pbuf;
    ErlNifBinary bin;

    if (argc != 3)
        return std::make_pair(-1, false);

    bool is_bin = (bool)enif_is_binary(env, argv[0]);

    if (is_bin) {
        if (!enif_inspect_binary(env, argv[0], &bin))
            return std::make_pair(-1, false);
        pbuf = maybe_copy_bin(bin, buf, sizeof(buf));
    } else if (enif_get_string(env, argv[0], buf, sizeof(buf), ERL_NIF_LATIN1) > 0)
        pbuf = buf;
    else
        return std::make_pair(-1, false);

    time_t t;

    if (enif_get_long(env, argv[1], &i0))
        t = i0; // If param is integer 
    else if (enif_get_tuple (env, argv[1], &arity, &time_tup) // Arity is 3 here
          && enif_get_long(env, time_tup[0], &i0)
          && enif_get_long(env, time_tup[1], &i1)) // we don't decode 3rd int (usec)
        t = time_t(i0 * 1000000LL + i1);
    else
        return std::make_pair(-1, false);

    // i array contains result of erlang:now()
    bool   utc = (bool)enif_is_identical(argv[2], ATOM_UTC);
    struct tm tm;

  #if defined(_WIN32) || defined(_WIN64)
    if (utc)
        gmtime_s(&tm, &t);
    else
        localtime_s(&tm, &t);
  #else
    if (utc)
        gmtime_r(&t, &tm);
    else
        localtime_r(&t, &tm);
  #endif

    return std::make_pair((int)strftime(res, sz, pbuf, &tm), is_bin);
}

#ifdef ERLCFG_TEST
int main(int argc, char* argv[]) {
  if (argc < 1) {
    printf("Usage %s 'StringWith ${var} or $var' ...\n", argv[0]);
    exit(1);
  }

  for (int i = 1; i < argc; ++i) {
    std::string s(argv[i]);
    replace_env_vars(s);
    printf("%s => %s\n", argv[i], s.c_str());
  }

  return 0;
}

#endif