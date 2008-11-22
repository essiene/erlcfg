-module(test_erlcfg_interp).
-include_lib("eunit/include/eunit.hrl").


new_test() ->
    ?assertEqual([], erlcfg_interp:new()).

eval_val_with_empty_interp_test() ->
    Interp = erlcfg_interp:new(),
    Expected = {Interp, foo},
    ?assertEqual(Expected, erlcfg_interp:eval(Interp, {val, foo, nil})).

eval_val_with_nonempty_interp_test() ->
    Interp = [{foo, bar}, {bar, baz}],
    Expected = {Interp, 5},
    ?assertEqual(Expected, erlcfg_interp:eval(Interp, {val, 5, nil})).

eval_set_test()  ->
    Interp = erlcfg_interp:new(),
    Expected = {[{foo, bar}], bar},
    ?assertEqual(Expected, erlcfg_interp:eval(Interp, {set, foo, bar})).

eval_set_no_parent_test()  ->
    Interp = erlcfg_interp:new(),
    {Interp1, bar} = erlcfg_interp:eval(Interp, {set, foo, bar}),
    Expected = {not_found, foo.foo},
    ?assertThrow(Expected, erlcfg_interp:eval(Interp1, {set, foo.foo.bar, bar})).
