-module(test_erlcfg_lexer).
-include_lib("eunit/include/eunit.hrl").


single_integer_test() ->
    ?assertEqual({ok, [{integer, 1, 1}], 1}, erlcfg_lexer:string("1")),
    ?assertEqual({ok, [{integer, 1, 5}], 1}, erlcfg_lexer:string("+5")),
    ?assertEqual({ok, [{integer, 1, 15}], 1}, erlcfg_lexer:string("15")),
    ?assertEqual({ok, [{integer, 1, -15}], 1}, erlcfg_lexer:string("-15")),
    ?assertEqual({ok, [{integer, 1, 15111}], 1}, erlcfg_lexer:string("+15111")),
    ?assertEqual({ok, [{integer, 1, 15111}], 1}, erlcfg_lexer:string("15111")).

multiple_integer_test() ->
    ?assertEqual({ok, [{integer, 1, 1}, {integer, 1, 2}, {integer, 1, 3}], 1}, erlcfg_lexer:string("1 2 3")),
    ?assertEqual({ok, [{integer, 1, 1}, {integer, 1, 20}, {integer, 1, -30}], 1}, erlcfg_lexer:string("+1 20 -30")),
    ?assertEqual({ok, [{integer, 1, 15111}, {integer, 2, -20}, {integer, 3, -111}], 3}, erlcfg_lexer:string("+15111\n-20\n-111")).

float_test() ->
    ?assertEqual({ok, [{float, 1, 1.0}], 1}, erlcfg_lexer:string("1.0")),
    ?assertEqual({ok, [{float, 1, 1.0}], 1}, erlcfg_lexer:string("+1.0")),
    ?assertEqual({ok, [{float, 1, -1.0}], 1}, erlcfg_lexer:string("-1.0")),
    ?assertEqual({ok, [{float, 1, 1.12e10}], 1}, erlcfg_lexer:string("+1.12e10")),
    ?assertEqual({ok, [{float, 1, 1.12e-10}], 1}, erlcfg_lexer:string("+1.12e-10")),
    ?assertEqual({ok, [{float, 1, -151.121e-159}], 1}, erlcfg_lexer:string("-151.121e-159")).

multiple_float_test() ->
    ?assertEqual({ok, [{float, 1, 1.0}, {float, 1, -2.11}, {float, 1, -3.0e-15}], 1}, erlcfg_lexer:string("+1.0 -2.11 -3.0e-15")),
    ?assertEqual({ok, [{float, 1, -1.1}, {float, 1, 20.0}, {float, 1, -30.1}], 1}, erlcfg_lexer:string("-1.1 +20.0 -30.1")),
    ?assertEqual({ok, [{float, 1, 15111.0}, {float, 2, -20.0}, {float, 3, -111.0}], 3}, erlcfg_lexer:string("+15111.0\n-20.0\n-111.0")).

