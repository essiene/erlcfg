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

-module(erlcfg_schema).
-export([
        new/1,
        combine/2,
        validate/2
    ]).
-include("schema.hrl").


new(FileName) ->
    {ok, Binary} = file:read_file(FileName),
    String = binary_to_list(Binary),
    {ok, TokenList, _LineCount} = erlcfg_schema_lexer:string(String),
    {ok, Ast} = erlcfg_schema_parser:parse(TokenList),
    {ok, Types} = erlcfg_schema_analyser1:analyse(Ast),
    {ok, erlcfg_schema_analyser2:analyse(Ast, Types)}.

combine(nil, nil) ->
    [];
combine(nil, Schema) ->
    Schema;
combine(Schema, nil) ->
    Schema;
combine(Schema1, Schema2) when is_list(Schema1), is_list(Schema2) ->
    lists:append([Schema1, Schema2]);
combine(Schema1, Schema2) when is_map(Schema1), is_map(Schema2) ->
    maps:merge(Schema1, Schema2).

validate(nil, Config) ->
    {ok, Config};
validate(SchemaTable, {erlcfg_data, {c, '', Data}} = Config) ->
    try
        Res = validate2(SchemaTable, Config),
        check_unique_and_undefined(Data, [], to_map(SchemaTable)),
        Res
    catch throw:Error ->
        {error, Error}
    end.

validate2(SchemaTable, Config) when is_list(SchemaTable) ->
    lists:foldl(
        fun(Key, Info, {ok, Cfg}) -> validate3(Key, Info, Cfg) end,
        {ok, Config}, SchemaTable);

validate2(SchemaTable, Config) when is_map(SchemaTable) ->
    maps:fold(
        fun(Key, Info, {ok, Cfg}) -> validate3(Key, Info, Cfg) end,
        {ok, Config}, SchemaTable).

validate3(Key, _D = #declaration{type=Tp, attrs=Attrs, validator=Val}, Config) ->
    V = case erlcfg_data:raw_get(Key, Config) of
        {error, _} ->
            case ensure_raw_default(Tp, Attrs#attrs.default) of
                ?ERLCFG_SCHEMA_NIL ->
                    throw([ 
                        {node, Key},
                        {expected_type, Val#validator.type},
                        {value, {error, required_value_no_default}}
                        ]);
                Val0 ->
                    Val0
            end;
        Val1 ->
            Val1
    end,
    if Tp =:= int; Tp =:= float ->
        Mn = Attrs#attrs.min,
        Mn =/= undefined andalso V < Mn andalso
            throw({value_below_min, [{node, Key}, {min, Mn}, {value, V}]}),
        Mx = Attrs#attrs.max,
        Mx =/= undefined andalso V > Mx andalso
            throw({value_above_max, [{node, Key}, {max, Mx}, {value, V}]});
    true ->
        ok
    end,
    validate_type(Key, V, Val, Config).

validate_type(Key, Value, Validator, Config) when is_tuple(Config), element(1,Config)==erlcfg_data ->
    Test = Validator#validator.test,
    case Test(Value) of
        false ->
            throw([
                    {node, Key}, 
                    {expected_type, Validator#validator.type}, 
                    {value, Value}
                  ]);
        true ->
            case Config:create(Key, Value) of
                {error, Reason} ->
                    throw(Reason);
                Config1 -> 
                    {ok, Config1}
            end
    end.


% Default values for strings will come in as a regular erlang
% string a.k.a list, but we need it to be in binary to conform
% with the rest of the system else the typechecking will bork.
ensure_raw_default(string, Value) when is_list(Value) -> 
    list_to_binary(Value);
ensure_raw_default(_, Value) -> 
    Value.

% Check that unique keys are indeed unique in Data, and also
% check that there are no keys that are not present in the schema
check_unique_and_undefined(Data, NameScope, Schema) when is_map(Schema) ->
    Root = lists:reverse(NameScope),
    check_valid(Data, Root, Schema).

check_valid([], _Data, _Schema) ->
    ok;
check_valid([{c, Name, Children}|T], Root, Schema) ->
    Option = append_root(Root, Name, list),
    check_valid(Children, Option, Schema),
    check_valid(T, Root, Schema);
check_valid([{d, Name, _Value}|T],   Root, Schema) ->
    Option = append_root(Root, Name, atom),
    case maps:find(Option, Schema) of
        {ok, #declaration{attrs = #attrs{unique = true}}} ->
            % Find if there are any remaining keys with the same name:
            [] =/= [I || I = {d, N, _} <- T, N =:= Name]
                andalso throw({found_non_unique_key, Option});
        {ok, #declaration{attrs = #attrs{unique = false}}} ->
            ok;
        error ->
            throw({not_found, Option})
    end,
    check_valid(T, Root, Schema).

append_root([],   Name, list) when is_atom(Name) -> atom_to_list(Name);
append_root([],   Name, atom) when is_atom(Name) -> Name;
append_root(Root, Name, list) when is_list(Root), is_atom(Name) ->
    Root ++ [$. | atom_to_list(Name)];
append_root(Root, Name, atom) when is_list(Root), is_atom(Name) ->
    Key = Root ++ [$. | atom_to_list(Name)],
    try   list_to_existing_atom(Key)
    catch error:badarg -> throw({invalid_key, Key})
    end.

to_map(L) when is_list(L) -> maps:from_list(L);
to_map(M) when is_map(M)  -> M.

