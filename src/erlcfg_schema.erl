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
    % Default values for strings will come in as a regular erlang
    % string a.k.a list, but we need it to be in binary to conform
    % with the rest of the system else the typechecking will bork.
    Default = case is_list(Default0) of
        true ->
            list_to_binary(Default0);
        false ->
            Default0
    end,
    Value = Config:raw_get(Key, Default),
    Test = Validator#validator.test,
    case Test(Value) of
        false ->
            {error, {
                        {node, Key}, 
                        {expected_type, Validator#validator.type}, 
                        {value, Value}
                    }
            };
        true ->
            %Config:set(Key, Value),
            validate(Rest, Config)
    end.
