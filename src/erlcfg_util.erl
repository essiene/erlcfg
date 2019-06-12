%% 
%% Copyright (c) 2014, Serge Aleynikov
%% All rights reserved.
%% 
%% Redistribution and use in source and binary forms, 
%% with or without modification, are permitted 
%% provided that the following conditions are met:
%%
%%    * Redistributions of source code must retain the 
%%      above copyright notice, this list of conditions 
%%      and the following disclaimer.
%%    * Redistributions in binary form must reproduce 
%%      the above copyright notice, this list of 
%%      conditions and the following disclaimer in the 
%%      documentation and/or other materials provided with 
%%      the distribution.
%%    * Neither the name "JsonEvents" nor the names of its 
%%      contributors may be used to endorse or promote 
%%      products derived from this software without 
%%      specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND 
%% CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED 
%% WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
%% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR 
%% PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT 
%% HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, 
%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES 
%% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE 
%% GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR 
%% BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF 
%% LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT 
%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF 
%% THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY 
%% OF SUCH DAMAGE.
%% 
-module(erlcfg_util).
-author('saleyn@gmail.com').
-on_load(init/0).

-export([strftime/2, strftime/3, strptime/2, pathftime/2, pathftime/3, strenv/1]).

-define(LIBNAME, "erlcfg_nifs").

%%------------------------------------------------------------------------------
%% External functions
%%------------------------------------------------------------------------------

%% @doc Substitute formatted date/time in a string using C strftime() function.
-spec strftime(Fmt :: string() | binary(), Now::erlang:timestamp()) ->
        string() | binary().
strftime(Format, Now) ->
    strftime(Format, Now, utc).

%% @doc Substitute formatted date/time in a string using C strftime() function.
-spec strftime(Fmt :: string() | binary(), Now::erlang:timestamp(), utc | local) ->
        string() | binary().
strftime(Format, _Now, _Utc) ->
    check(Format).

%% @doc Substitute formatted date/time in a path using C strftime() function.
%% If the path begins with `"~"', it'll be replaces with the value of `${HOME}'.
%% All environment variables in the path in the form `${VAR}' will be substituted
%% with evaluated values.
-spec pathftime(Fmt :: string() | binary(), Now::erlang:timestamp()) ->
        string() | binary().
pathftime(Format, Now) ->
    pathftime(Format, Now, utc).

%% @doc Substitute formatted date/time in a path using C strftime() function.
-spec pathftime(Fmt :: string() | binary(), Now::erlang:timestamp(), utc | local) ->
        string() | binary().
pathftime(Format, _Now, _Utc) ->
    check(Format).

%% @doc Substitute formatted date/time in a string using C strftime() function.
%% All environment variables in the string in the form `${VAR}' will be substituted
%% with evaluated values.
-spec strptime(Input::string() | binary(), Fmt::string() | binary()) ->
        {Time::calendar:datetime(), Processed::integer()}.
strptime(Input, _Format) ->
    check(Input).

-spec strenv(Input :: string() | binary()) -> string() | binary().
strenv(Input) when is_list(Input); is_binary(Input) ->
    check(Input).

   
%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------

init() ->
    SoName = getdir(?LIBNAME),
    erlang:load_nif(SoName, 0).

getdir(LibName) ->
    case code:priv_dir(erlcfg) of
    {error, bad_name} ->
        case code:which(?MODULE) of
        Filename when is_list(Filename) ->
            filename:join([filename:dirname(Filename), "../priv", LibName]);
        _ ->
            filename:join("../priv", LibName)
        end;
    Dir ->
        filename:join(Dir, LibName)
    end.

check(_) ->
    throw({library_not_loaded, ?LIBNAME}).
 
