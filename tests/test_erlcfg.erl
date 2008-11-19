-module(test_erlcfg).
-include_lib("eunit/include/eunit.hrl").


get_empty_test() ->
    ?assertEqual({value, {["foo"], bar}}, erlcfg:get([], ["foo"], bar)).
