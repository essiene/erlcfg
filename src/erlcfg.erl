-module(erlcfg).
-export([
        new/0,
        new/1,
        new/2
    ]).
-include("erlcfg.hrl").

new() ->
    {ok, erlcfg_data:new(erlcfg_node:new())}.

new(FileName) ->
    new(FileName, false).

new(FileName, ValidateSchema) ->
    {ok, Binary} = file:read_file(FileName),
    String = binary_to_list(Binary),
    {ok, TokenList, _LineCount} = erlcfg_lexer:string(String),
    {ok, Ast} = erlcfg_parser:parse(TokenList),
    {ok, InterpState} = erlcfg_interp:interpret(Ast),
    Unvalidated = erlcfg_data:new(InterpState#interp.node),
    case ValidateSchema of
        false ->
            {ok, Unvalidated};
        true ->
            erlcfg_schema:validate(InterpState#interp.schema_table, Unvalidated)
    end.
