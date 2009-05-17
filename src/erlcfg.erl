-module(erlcfg).
-export([
        new/0,
        new/1
    ]).
-include("erlcfg.hrl").

new() ->
    erlcfg_data:new(erlcfg_node:new()).

new(FileName) ->
    case file:read_file(FileName) of
        {ok, Binary} ->
            String = binary_to_list(Binary),
            case erlcfg_lexer:string(String) of
                {ok, TokenList, _LineCount} ->
                    case erlcfg_parser:parse(TokenList) of
                        {ok, Ast} ->
                            InterpState = erlcfg_interp:eval(Ast),
                            erlcfg_data:new(InterpState#interp.node);
                        Other ->
                            Other
                    end;
                Other ->
                    Other
            end;
        {error, Reason} ->
            {error, Reason}
    end.
