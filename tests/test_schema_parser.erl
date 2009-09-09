-module(test_schema_parser).
-include_lib("eunit/include/eunit.hrl").
-include("schema.hrl").

parse_int_declaration_test() ->
    Tokens = [{datatype, 1, int}, {atom, 1, foo}, {';', 1}],
    Result = erlcfg_schema_parser:parse(Tokens),
    Expected = {ok, [{declaration, int, foo}]},
    ?assertEqual(Expected, Result).

parse_float_declaration_test() ->
    Tokens = [{datatype, 1, float}, {atom, 1, foo}, {';', 1}],
    Result = erlcfg_schema_parser:parse(Tokens),
    Expected = {ok, [{declaration, float, foo}]},
    ?assertEqual(Expected, Result).

parse_bool_declaration_test() ->
    Tokens = [{datatype, 1, bool}, {atom, 1, foo}, {';', 1}],
    Result = erlcfg_schema_parser:parse(Tokens),
    Expected = {ok, [{declaration, bool, foo}]},
    ?assertEqual(Expected, Result).
