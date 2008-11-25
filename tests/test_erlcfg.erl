-module(test_erlcfg).
-include_lib("eunit/include/eunit.hrl").

new_test() ->
    Config = erlcfg:new("flat.conf"),
    ?assertEqual(1, Config:get(one_int)),
    ?assertEqual(10.038e-10, Config:get(two_float)),
    ?assertEqual(atom3, Config:get(three_atom)),
    ?assertEqual(awesome@quoted.atom, Config:get(four_quoted_atom)),
    ?assertEqual("A string", Config:get(five_string)).

new_variable_test() ->
    Config = erlcfg:new("flat.conf"),
    ?assertEqual(1, Config:get(six_variable)),
    ?assertEqual(awesome@quoted.atom, Config:get(seven_variable)).
