-module(erlcfg).
-export([
        new/0,
        new/1,
        check/1
    ]).
-include("erlcfg.hrl").

new() ->
    {ok, erlcfg_data:new(erlcfg_node:new())}.

new(FileName) ->
    {ok, Binary} = file:read_file(FileName),
    String = binary_to_list(Binary),
    {ok, TokenList, _LineCount} = erlcfg_lexer:string(String),
    {ok, Ast} = erlcfg_parser:parse(TokenList),
    InterpState = erlcfg_interp:eval(Ast),
    {ok, erlcfg_data:new(InterpState#interp.node)}.

check(FileName) ->
    {ok, Binary} = file:read_file(FileName),
    String = binary_to_list(Binary),
    {ok, TokenList, _LineCount} = erlcfg_schema_lexer:string(String),
    {ok, Ast} = erlcfg_schema_parser:parse(TokenList),
    {ok, Types} = erlcfg_schema_analyser1:analyse(Ast),
    erlcfg_schema_analyser2:analyse(Ast, Types).
