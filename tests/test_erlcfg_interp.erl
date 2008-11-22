-module(test_erlcfg_interp).
-include_lib("eunit/include/eunit.hrl").


new_test() ->
    ?assertEqual([], erlcfg_interp:new()).
