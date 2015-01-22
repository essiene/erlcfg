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

-module(erlcfg).
-export([
        new/0,
        new/1,
        new/2,
        new/3
    ]).
-include("erlcfg.hrl").

-type config() :: {erlcfg_data, tuple()}.

-export_type([config/0]).

new() ->
    {ok, erlcfg_data:new(erlcfg_node:new())}.

new(FileName) ->
    new(FileName, false, []).

new(FileName, ValidateSchema) when is_boolean(ValidateSchema) ->
    new(FileName, ValidateSchema, []).

new(FileName, Validate, Macros) when is_boolean(Validate), is_list(Macros) ->
    MacrosMap = maps:from_list(Macros),
    new(FileName, Validate, MacrosMap);
new(FileName, Validate, Macros) when is_boolean(Validate), is_map(Macros) ->
    {ok, Binary} = file:read_file(FileName),
    new(Binary, Validate, Macros);
new(CfgData, Validate, Macros) when is_binary(CfgData), is_boolean(Validate), is_map(Macros) ->
    String = binary_to_list(CfgData),
    {ok, TokenList, _LineCount} = erlcfg_lexer:string(String),
    {ok, Ast} = erlcfg_parser:parse(TokenList),
    {ok, #interp{schema_table=Schema, node=Node}} = erlcfg_interp:interpret(Ast, Macros),
    validate(Validate, Schema, erlcfg_data:new(Node)).

validate(false, _Schema, Unvalidated) ->
    {ok, Unvalidated};
validate(true,  Schema, Unvalidated) ->
    erlcfg_schema:validate(Schema, Unvalidated).
