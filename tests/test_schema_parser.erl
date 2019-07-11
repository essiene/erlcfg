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

-module(test_schema_parser).
-include_lib("eunit/include/eunit.hrl").
-include("schema.hrl").

parse_int_declaration_test() ->
    Tokens = [{datatype, 1, int}, {atom, 1, foo}, {';', 1}],
    Result = erlcfg_schema_parser:parse(Tokens),
    Expected = {ok, [#declaration{type=int, name=foo, attrs=[], validator=#validator{type=undefined}}]},
    ?assertEqual(Expected, Result).

parse_int_declaration_with_default_test() ->
    Tokens = [{datatype, 1, int}, {atom, 1, foo}, {'=', 1}, {integer, 1, 2}, {';', 1}],
    Result = erlcfg_schema_parser:parse(Tokens),
    Expected = {ok, [#declaration{type=int, name=foo, attrs=[{default, 2}], validator=#validator{type=undefined}}]},
    ?assertEqual(Expected, Result).

parse_float_declaration_test() ->
    Tokens = [{datatype, 1, float}, {atom, 1, foo}, {';', 1}],
    Result = erlcfg_schema_parser:parse(Tokens),
    Expected = {ok, [#declaration{type=float, name=foo, attrs=[], validator=#validator{}}]},
    ?assertEqual(Expected, Result).

parse_bool_declaration_test() ->
    Tokens = [{datatype, 1, bool}, {atom, 1, foo}, {';', 1}],
    Result = erlcfg_schema_parser:parse(Tokens),
    Expected = {ok, [#declaration{type=bool, name=foo, attrs=[], validator=#validator{}}]},
    ?assertEqual(Expected, Result).

parse_atom_declaration_test() ->
    Tokens = [{datatype, 1, atom}, {atom, 1, foo}, {';', 1}],
    Result = erlcfg_schema_parser:parse(Tokens),
    Expected = {ok, [#declaration{type=atom, name=foo, attrs=[], validator=#validator{type=undefined}}]},
    ?assertEqual(Expected, Result).

parse_string_declaration_test() ->
    Tokens = [{datatype, 1, string}, {atom, 1, foo}, {';', 1}],
    Result = erlcfg_schema_parser:parse(Tokens),
    Expected = {ok, [#declaration{type=string, name=foo, attrs=[], validator=#validator{}}]},
    ?assertEqual(Expected, Result).

parse_typedef_test() ->
    Tokens = [{keyword_type, 1, type}, 
        {atom, 1, newtype},
        {'=', 1}, 
            {atom, 1, foo}, 
                {'|', 1}, 
            {atom, 1, bar}, 
         {';', 1}
     ],
    Result = erlcfg_schema_parser:parse(Tokens),
    Expected = {ok, [{typedef, newtype, {cons, foo, {cons, bar, nil}}
            }
    ]},
    ?assertEqual(Expected, Result).

parse_empty_block_test() ->
    Tokens = [
        {atom, 1, block1},
        {'{', 1},
        {'}', 1}
    ],

    Result = erlcfg_schema_parser:parse(Tokens),
    Expected = {ok, [{block, block1, []}]},
    ?assertEqual(Expected, Result).

parse_block_with_declarations_test() ->
    Tokens = [
        {atom, 1, block1},
        {'{', 1},
            {datatype, 1, string}, {atom, 1, foo}, {';', 1},
            {datatype, 1, int}, {atom, 1, bar}, {';', 1},
        {'}', 1}
    ],

    Result = erlcfg_schema_parser:parse(Tokens),
    Expected = {ok, [{block, block1, [
                    #declaration{type=string, name=foo, attrs=[]},
                    #declaration{type=int, name=bar, attrs=[]}
                ]}]},
    ?assertEqual(Expected, Result).

parse_block_with_nested_block_test() ->
    Tokens = [
        {atom, 1, block1},
        {'{', 1},
            {datatype, 1, string}, {atom, 1, foo}, {';', 1},
            {datatype, 1, int}, {atom, 1, bar}, {';', 1},
            {atom, 1, block1},
            {'{', 1},
                {datatype, 1, string}, {atom, 1, foo}, {';', 1},
                {datatype, 1, int}, {atom, 1, bar}, {';', 1},
            {'}', 1},
        {'}', 1}
    ],

    Result = erlcfg_schema_parser:parse(Tokens),
    Expected = {ok, [{block, block1, [
                    #declaration{type=string, name=foo, attrs=[]},
                    #declaration{type=int,    name=bar, attrs=[]},
                    {block, block1, [
                        #declaration{type=string, name=foo, attrs=[]},
                        #declaration{type=int,    name=bar, attrs=[]}
                    ]}
                ]}]},
    ?assertEqual(Expected, Result).

parse_complex_with_typedefs_and_blocks_test() ->
    Tokens = [
        {keyword_type, 1, type}, 
            {atom, 1, newtype1},
            {'=', 1}, 
                {atom, 1, foo}, 
                    {'|', 1}, 
                {atom, 1, bar}, 
             {';', 1},

        {atom, 1, block1},
        {'{', 1},
            {datatype, 1, string}, {atom, 1, foo}, {';', 1},
            {datatype, 1, int}, {atom, 1, bar}, {';', 1},
            {atom, 1, block1},
            {'{', 1},
                {datatype, 1, string}, {atom, 1, foo}, {';', 1},
                {datatype, 1, int}, {atom, 1, bar}, {';', 1},
            {'}', 1},
        {'}', 1},

        {keyword_type, 1, type}, 
            {atom, 1, newtype2},
            {'=', 1}, 
                {atom, 1, moo}, 
                    {'|', 1}, 
                {atom, 1, meh}, 
             {';', 1}

    ],

    Result = erlcfg_schema_parser:parse(Tokens),
    Expected = {ok, [
            {typedef, newtype1, {cons, foo, {cons, bar, nil}}},
            {block, block1, [
                    #declaration{type=string, name=foo, attrs=[]},
                    #declaration{type=int,    name=bar, attrs=[]},
                    {block, block1, [
                        #declaration{type=string, name=foo, attrs=[]},
                        #declaration{type=int,    name=bar, attrs=[]}
                    ]}
             ]},
            {typedef, newtype2, {cons, moo, {cons, meh, nil}}}
    ]},
    ?assertEqual(Expected, Result).

parse_list_of_int_declaration_test() ->
    Tokens = [{'[', 1}, {datatype, 1, string}, {']', 1}, {atom, 1, foo}, {';', 1}],
    Result = erlcfg_schema_parser:parse(Tokens),
    Expected = {ok, [#declaration{type={listof, string}, name=foo, attrs=[]}]},
    ?assertEqual(Expected, Result).


parse_list_of_int_declaration_with_default_test() ->
    Tokens = [{'[', 1}, {datatype, 1, string}, {']', 1}, {atom, 1, foo}, {'=',
            1}, {'[', 1}, {string, 1, <<"2">>}, {',', 1}, {string, 1, <<"3">>}, {']', 1}, {';', 1}],
    Result = erlcfg_schema_parser:parse(Tokens),
    Expected = {ok, [#declaration{type = #listof{type = string},
                      name = foo,
                      attrs = [{default,#cons{head = <<"2">>, tail =
                                        #cons{head = <<"3">>, tail = nil}}}]}]},
    ?assertEqual(Expected, Result).
