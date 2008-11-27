-module(test_erlcfg_interp).
-include_lib("eunit/include/eunit.hrl").


new_test() ->
    ?assertEqual({c, '', []}, erlcfg_interp:new()).

eval_val_with_empty_interp_test() ->
    Interp = erlcfg_interp:new(),
    Expected = {Interp, '', foo},
    ?assertEqual(Expected, erlcfg_interp:eval(Interp, '', {val, foo, nil})).

eval_val_with_nonempty_interp_test() ->
    Interp = {c, '', [{d, foo, bar}, {d, bar, baz}]},
    Expected = {Interp, '', 5},
    ?assertEqual(Expected, erlcfg_interp:eval(Interp, '', {val, 5, nil})).

eval_set_test()  ->
    Interp = erlcfg_interp:new(),
    Expected = {{c, '', [{d, foo, bar}]}, '', bar},
    ?assertEqual(Expected, erlcfg_interp:eval(Interp, '', {set, foo, bar})).

eval_set_nested_val_test()  ->
    Interp = erlcfg_interp:new(),
    Expected = {{c, '', [{d, foo, bar}]}, '', bar},
    ?assertEqual(Expected, erlcfg_interp:eval(Interp, '', {set, foo, {val, bar, nil}})).

eval_set_nested_get_test()  ->
    I = erlcfg_interp:new(),
    {I1, '', bar} = erlcfg_interp:eval(I, '', {set, foo, {val, bar, nil}}),
    {Interp, '', bar} = erlcfg_interp:eval(I1, '', {set, moo, {val, {get, foo, nil}, nil}}),

    Expected = {{c, '', [{d, foo, bar}, {d, moo, bar}]}, '', bar},
    ?assertEqual(Expected, {Interp, '', bar}).

eval_block_test() ->
    I = erlcfg_interp:new(),
    Result = erlcfg_interp:eval(I, '', {block, foo, noop}),

    Expected = {{c, '', [{c, foo, []}]}, foo, []},
    ?assertEqual(Expected, Result).

eval_endblock_test() ->
    I = erlcfg_interp:new(),
    {I1, foo, []} = erlcfg_interp:eval(I, '', {block, foo, noop}),
    Result = erlcfg_interp:eval(I1, foo, {endblock, noop, noop}),

    Expected = {{c, '', [{c, foo, []}]}, '', foo},
    ?assertEqual(Expected, Result).

eval_cons_test() ->
    Interp = erlcfg_interp:new(),
    {Interp, '', Value} = erlcfg_interp:eval(Interp, '', nil),
    ?assertEqual(Value, []),

    {Interp, '', Value1} = erlcfg_interp:eval(Interp, '', {cons, {val, 1, noop}, nil}),
    ?assertEqual(Value1, [1]),

    {Interp, '', Value2} = erlcfg_interp:eval(Interp, '', {cons, {val, 2, noop}, {cons, {val, 1, noop}, nil}}),
    ?assertEqual(Value2, [2,1]).

eval_set_no_parent_test()  ->
    Interp = erlcfg_interp:new(),
    {Interp1, '', bar} = erlcfg_interp:eval(Interp, '', {set, foo, bar}),
    Expected = {not_found, foo.foo},
    ?assertThrow(Expected, erlcfg_interp:eval(Interp1, '', {set, foo.foo.bar, bar})).

eval_illegal_command_test()  ->
    Interp = erlcfg_interp:new(),
    Expected = {illegal_command, {read, moo, noop}},
    ?assertThrow(Expected, erlcfg_interp:eval(Interp, '', {read, moo, noop})).
