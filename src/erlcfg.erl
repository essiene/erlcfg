-module(erlcfg).
-export([
        new/1
    ]).


new(FileName) ->
    case file:read_file(FileName) of
        {ok, Binary} ->
            String = binary_to_list(Binary),
            case erlcfg_lexer:string(String) of
                {ok, TokenList, _LineCount} ->
                    case erlcfg_parser:parse(TokenList) of
                        {ok, Ast} ->
                            Data = erlcfg_ast:traverse(Ast),
                            erlcfg_data:new(Data);
                        Other ->
                            Other
                    end;
                Other ->
                    Other
            end;
        {error, Reason} ->
            {error, Reason}
    end.
