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
combine(Schema1, Schema2) ->
    lists:append([Schema1, Schema2]).

validate(nil, Config) ->
    {ok, Config};
validate([], Config) ->
    {ok, Config};
validate([{Key, {Default0, #validator{}=Validator}}|Rest], Config) ->
    Default = ensure_raw_default(Default0),
    Value = Config:raw_get(Key, Default),

    case Value of
       {error, Reason} ->
            {error, Reason};
       ?ERLCFG_SCHEMA_NIL ->
            {error, [ 
                {node, Key},
                {expected_type, Validator#validator.type},
                {value, {error, not_given}}
                ]
            };
       _NonNull ->
            validate_type(Key, Value, Validator, Rest, Config)
    end.

validate_type(Key, Value, Validator, Rest, Config) ->
    Test = Validator#validator.test,
    case Test(Value) of
        false ->
            {error, [
                        {node, Key}, 
                        {expected_type, Validator#validator.type}, 
                        {value, Value}
                    ]
            };
        true ->
            case Config:create(Key, Value) of
                {error, Reason} ->
                    {error, Reason};
                Config1 -> 
                    validate(Rest, Config1)
            end
    end.


% Default values for strings will come in as a regular erlang
% string a.k.a list, but we need it to be in binary to conform
% with the rest of the system else the typechecking will bork.
ensure_raw_default(Value) when is_list(Value) -> 
    list_to_binary(Value);
ensure_raw_default(Value) -> 
    Value.
