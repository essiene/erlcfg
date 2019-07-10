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

-module(test_erlcfg_parser).
-include_lib("eunit/include/eunit.hrl").
-include("erlcfg.hrl").

parse_bool_assignment_test() ->
    Tokens = [{atom, 1, foo}, {'=', 1}, {bool, 1, true}, {';', 1}],
    Result = erlcfg_parser:parse(Tokens),
    Expected = {ok, [{set, foo, true}]},
    ?assertEqual(Expected, Result).

parse_integer_assignment_test() ->
    Tokens = [{atom, 1, foo}, {'=', 1}, {integer, 1, 51}, {';', 1}],
    Result = erlcfg_parser:parse(Tokens),
    Expected = {ok, [{set, foo, 51}]},
    ?assertEqual(Expected, Result).

parse_float_assignment_test() ->
    Tokens = [{atom, 1, foo}, {'=', 1}, {float, 1, -51.0e-10}, {';', 1}],
    Result = erlcfg_parser:parse(Tokens),
    Expected = {ok, [{set, foo, -51.0e-10}]},
    ?assertEqual(Expected, Result).

parse_atom_assignment_test() ->
    Tokens = [{atom, 1, foo}, {'=', 1}, {atom, 1, foo51}, {';', 1}],
    Result = erlcfg_parser:parse(Tokens),
    Expected = {ok, [{set, foo, foo51}]},
    ?assertEqual(Expected, Result).

parse_quoted_atom_assignment_test() ->
    Tokens = [{atom, 1, foo}, {'=', 1}, {quoted_atom, 1, '.foo@51'}, {';', 1}],
    Result = erlcfg_parser:parse(Tokens),
    Expected = {ok, [{set, foo, '.foo@51'}]},
    ?assertEqual(Expected, Result).

parse_string_assignment_test() ->
    Tokens = [{atom, 1, foo}, {'=', 1}, {string, 1, <<"A String">>}, {';', 1}],
    Result = erlcfg_parser:parse(Tokens),
    Expected = {ok, [{set, foo, <<"A String">>}]},
    ?assertEqual(Expected, Result).

parse_variable_assignment_test() ->
    Tokens = [{atom, 1, foo}, {'=', 1}, {variable, 1, 'foo.bar'}, {';', 1}],
    Result = erlcfg_parser:parse(Tokens),
    Expected = {ok, [{set, foo, {get, 'foo.bar'}}]},
    ?assertEqual(Expected, Result).

parse_single_block_assignment_test() ->
    Tokens = [{atom, 1, foo}, {'{', 1}, {atom, 1, foo}, {'=', 1}, {atom, 1, moo}, {';', 1}, {'}', 1}],
    Result = erlcfg_parser:parse(Tokens),
    Expected = {ok, [{block, foo, [{set, foo, moo}]}]},
    ?assertEqual(Expected, Result).

parse_empty_list_assignment_test() ->
    Tokens = [{atom, 1, foo}, {'=', 1}, {'[', 1}, {']', 1}, {';', 1}],
    Result = erlcfg_parser:parse(Tokens),
    Expected = {ok, [{set, foo, {list, nil}}]},
    ?assertEqual(Expected, Result).

parse_single_directive_test() ->
    Tokens = [{'@', 1}, {atom, 1, foo}, {'(', 1}, {atom, 1, 'path.schema'}, {')', 1}, {';', 1}],
    Result = erlcfg_parser:parse(Tokens),
    Expected = {ok, [{directive, foo, 'path.schema'}]},
    ?assertEqual(Expected, Result).

parse_single_value_list_assignment_test() ->
    Tokens = [{atom, 1, foo}, {'=', 1}, {'[', 1}, {atom, 1, bar}, {']', 1}, {';', 1}],
    Result = erlcfg_parser:parse(Tokens),
    Expected = {ok, [{set, foo, {list, {cons, bar, nil}}}]},
    ?assertEqual(Expected, Result).

parse_multiple_value_list_assignment_test() ->
    Tokens = [{atom, 1, foo}, {'=', 1}, 
        {'[', 1}, 
            {atom, 1, bar}, 
            {',', 1},
            {integer, 1, 5},
            {',', 1},
            {float, 1, 0.59},
            {',', 1},
            {string, 1, <<"A String">>},
        {']', 1}, {';', 1}],
    Result = erlcfg_parser:parse(Tokens),
    Expected = {ok, [{set, foo, {list, {cons, bar, {cons, 5, {cons, 0.59, {cons, <<"A String">>, nil}}}}}}]},
    ?assertEqual(Expected, Result).

parse_single_nested_blocks_assignment_test() ->
    Tokens = [
        {atom, 1, foo}, {'{', 1}, 
            {atom, 1, foo}, {'=', 1}, {atom, 1, moo}, {';', 1}, 
            {atom, 1, foo1}, {'=', 1}, {atom, 1, moo1}, {';', 1}, 
            {atom, 1, foo2}, {'{', 1},
                {atom, 1, foo}, {'=', 1}, {atom, 1, moo}, {';', 1}, 
                {atom, 1, foo1}, {'=', 1}, {atom, 1, moo1}, {';', 1}, 
            {'}', 1},
        {'}', 1}
    ],
    Result = erlcfg_parser:parse(Tokens),
    Expected = {ok,[
            {block, foo, [ 
                    {set, foo, moo},
                    {set, foo1, moo1},
                    {block, foo2, [
                            {set, foo, moo},
                            {set, foo1, moo1}]
                    }]}]},
    ?assertEqual(Expected, Result).

parse_mixed_assignment_test() ->
    Tokens = [
        {atom, 1, foo}, {'=', 1}, {bool, 1, false}, {';', 1},
        {atom, 1, foo}, {'=', 1}, {integer, 1, 51}, {';', 1},
        {atom, 1, foo}, {'=', 1}, {float, 1, -51.0e-10}, {';', 1},
        {atom, 1, foo}, {'=', 1}, {atom, 1, foo51}, {';', 1},
        {atom, 1, foo}, {'=', 1}, {string, 1, <<"A String">>}, {';', 1},
        {atom, 1, foo}, {'=', 1}, {variable, 1, 'foo.bar'}, {';', 1}
    ],
    Result = erlcfg_parser:parse(Tokens),
    Expected = {ok, [
            {set, foo, false}, 
            {set, foo, 51},
            {set, foo, -51.0e-10},
            {set, foo, foo51}, 
            {set, foo, <<"A String">>}, 
            {set, foo, {get, 'foo.bar'}}]},
    ?assertEqual(Expected, Result).
