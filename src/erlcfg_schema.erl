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
