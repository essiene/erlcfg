%% 
%% Copyright (c) 2008-2010, Essien Ita Essien
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

-module(test_erlcfg_lexer).
-include_lib("eunit/include/eunit.hrl").
-export([spaces/0]).



single_bool_test() ->
    ?assertEqual({ok, [{bool, 1, true}], 1}, erlcfg_lexer:string("true")),
    ?assertEqual({ok, [{bool, 1, false}], 1}, erlcfg_lexer:string("false")).

multiple_bool_test() ->
    ?assertEqual({ok, [{bool, 1, true}, {bool, 1, true}, {bool, 1, true}], 1}, erlcfg_lexer:string("true true true")),
    ?assertEqual({ok, [{bool, 1, false}, {bool, 1, false}, {bool, 1, false}], 1}, erlcfg_lexer:string("false false false")),
    ?assertEqual({ok, [{bool, 1, true}, {bool, 2, false}, {bool, 2, true}, {bool, 2, false}, {bool, 3, false}], 3}, erlcfg_lexer:string("true\nfalse true false\nfalse")).


single_integer_test() ->
    ?assertEqual({ok, [{integer, 1, 1}], 1}, erlcfg_lexer:string("1")),
    ?assertEqual({ok, [{integer, 1, 5}], 1}, erlcfg_lexer:string("+5")),
    ?assertEqual({ok, [{integer, 1, 15}], 1}, erlcfg_lexer:string("15")),
    ?assertEqual({ok, [{integer, 1, -15}], 1}, erlcfg_lexer:string("-15")),
    ?assertEqual({ok, [{integer, 1, 15111}], 1}, erlcfg_lexer:string("+15111")),
    ?assertEqual({ok, [{integer, 1, 15111}], 1}, erlcfg_lexer:string("15111")).

multiple_integer_test() ->
    ?assertEqual({ok, [{integer, 1, 1}, {integer, 1, 2}, {integer, 1, 3}], 1}, erlcfg_lexer:string("1 2 3")),
    ?assertEqual({ok, [{integer, 1, 1}, {integer, 1, 20}, {integer, 1, -30}], 1}, erlcfg_lexer:string("+1 20 -30")),
    ?assertEqual({ok, [{integer, 1, 15111}, {integer, 2, -20}, {integer, 3, -111}], 3}, erlcfg_lexer:string("+15111\n-20\n-111")).

float_test() ->
    ?assertEqual({ok, [{float, 1, 1.0}], 1}, erlcfg_lexer:string("1.0")),
    ?assertEqual({ok, [{float, 1, 1.0}], 1}, erlcfg_lexer:string("+1.0")),
    ?assertEqual({ok, [{float, 1, -1.0}], 1}, erlcfg_lexer:string("-1.0")),
    ?assertEqual({ok, [{float, 1, 1.12e10}], 1}, erlcfg_lexer:string("+1.12e10")),
    ?assertEqual({ok, [{float, 1, 1.12e-10}], 1}, erlcfg_lexer:string("+1.12e-10")),
    ?assertEqual({ok, [{float, 1, -151.121e-159}], 1}, erlcfg_lexer:string("-151.121e-159")).

multiple_float_test() ->
    ?assertEqual({ok, [{float, 1, 1.0}, {float, 1, -2.11}, {float, 1, -3.0e-15}], 1}, erlcfg_lexer:string("+1.0 -2.11 -3.0e-15")),
    ?assertEqual({ok, [{float, 1, -1.1}, {float, 1, 20.0}, {float, 1, -30.1}], 1}, erlcfg_lexer:string("-1.1 +20.0 -30.1")),
    ?assertEqual({ok, [{float, 1, 15111.0}, {float, 2, -20.0}, {float, 3, -111.0}], 3}, erlcfg_lexer:string("+15111.0\n-20.0\n-111.0")).

atom_test() ->
    ?assertEqual({ok, [{atom, 1, foo}], 1}, erlcfg_lexer:string("foo")),
    ?assertEqual({ok, [{quoted_atom, 1, foo}], 1}, erlcfg_lexer:string("'foo'")),
    ?assertEqual({ok, [{quoted_atom, 1, '+1.0e-10'}], 1}, erlcfg_lexer:string("'+1.0e-10'")),
    ?assertEqual({ok, [{atom, 1, 'foo-dot-com'}], 1}, erlcfg_lexer:string("foo-dot-com")),
    ?assertEqual({ok, [{atom, 1, '_foo-dot_com'}], 1}, erlcfg_lexer:string("_foo-dot_com")).

multiple_atom_test() ->
    ?assertEqual({ok, [{atom, 1, foo}, {atom, 1, bar}, {atom, 1, baz}], 1}, erlcfg_lexer:string("foo bar baz")),
    ?assertEqual({ok, [{quoted_atom, 1, foo}, {atom, 1, bar}, {atom, 2, baz}], 2}, erlcfg_lexer:string("'foo' bar\n\tbaz")),
    ?assertEqual({ok, [{quoted_atom, 1, foo}, {quoted_atom, 1, bar}, {quoted_atom, 2, baz}], 2}, erlcfg_lexer:string("'foo' 'bar'\n\t'baz'")).

string_test() ->
    ?assertEqual({ok, [{string, 1, <<"foo">>}], 1}, erlcfg_lexer:string("\"foo\"")),
    ?assertEqual({ok, [{string, 1, <<"foo">>}], 1}, erlcfg_lexer:string("\"foo\"")),
    ?assertEqual({ok, [{string, 1, <<"+1.0e-10">>}], 1}, erlcfg_lexer:string("\"+1.0e-10\"")),
    ?assertEqual({ok, [{string, 1, <<"foo-dot-com">>}], 1}, erlcfg_lexer:string("\"foo-dot-com\"")),
    ?assertEqual({ok, [{string, 1, <<"_foo-dot_com and lots lost lots !!! more#@!">>}], 1}, erlcfg_lexer:string("\"_foo-dot_com and lots lost lots !!! more#@!\"")).

multiple_string_test() ->
    ?assertEqual({ok, [{string, 1, <<"foo">>}, {string, 1, <<"bar">>}, {string, 1, <<"baz">>}], 1}, erlcfg_lexer:string("\"foo\" \"bar\" \"baz\"")),
    ?assertEqual({ok, 
            [ 
                {string, 1, <<"foo bar">>}, 
                {string, 1, <<"bar">>}, 
                {string, 2, <<"baz frob">>}
            ], 2}, 
        
        erlcfg_lexer:string("\"foo bar\" \"bar\"\n\t\"baz frob\"")).

string_with_spaces_test() ->
    ?assertEqual({ok, [
                {string, 1, <<"foo with bar">>},
                {string, 1, <<"with a\ttab">>},
                {string, 1, <<"and \na \nnew line">>}
            ], 3},
        erlcfg_lexer:string("\"foo with bar\" \"with a\ttab\" \"and \na \nnew line\"")).

single_variable_test() ->
    ?assertEqual({ok, [{variable, 1, common.one}], 1}, erlcfg_lexer:string("$common.one")),
    ?assertEqual({ok, [{variable, 1, foo.bar}], 1}, erlcfg_lexer:string("$foo.bar")).

multiple_variable_test() ->
    ?assertEqual({ok, [{variable, 1, foo.one}, {variable, 1, foo.two}, {variable, 1, foo.three}], 1}, erlcfg_lexer:string("$foo.one $foo.two $foo.three")),
    ?assertEqual({ok, [{variable, 1, moo}, {variable, 1, one.two.three.four}, {variable, 1, 'foobar_.baz-bar'}], 1}, erlcfg_lexer:string("$moo $one.two.three.four $foobar_.baz-bar")).


spaces() -> 
    io:format("~p~n", [erlcfg_lexer:string("\"foo with bar\" \"with a\ttab\" \"and\na \nnew line\"")]).


whitespace_test() ->
    ?assertEqual({ok, [{integer, 2, 1}], 3}, erlcfg_lexer:string("#1 1 1\n1#111\n#1.0")),
    ?assertEqual({ok, [{integer, 2, 3}, {float, 2, -2.11e-15}, {atom, 2, foo_dot_com}, {string, 2, <<"a random.long#string">>}], 3}, erlcfg_lexer:string(" #A line of comments beginning with space\n+3 -2.11e-15 foo_dot_com \"a random.long#string\" #some comments\n#another line of comments")).

