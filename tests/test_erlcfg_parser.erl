-module(test_erlcfg_parser).
-include_lib("eunit/include/eunit.hrl").


parse_integer_assignment_test() ->
    Tokens = [{atom, 1, foo}, {'=', 1}, {integer, 1, 51}, {';', 1}],
    Result = erlcfg_parser:parse(Tokens),
    Expected = {ok, {set, {key, foo}, {val, 51}}},
    ?assertEqual(Expected, Result).
