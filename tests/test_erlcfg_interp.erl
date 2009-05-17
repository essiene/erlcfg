-module(test_erlcfg_interp).
-include_lib("eunit/include/eunit.hrl").
-include("erlcfg.hrl").


eval_nil_test() ->
    Expected = {c, '', []},
    ?assertEqual(Expected, erlcfg_interp2:eval(nil)).

eval_set_test()  ->
    Expected = {c, '', [{d, foo, bar}]},
    ?assertEqual(Expected, erlcfg_interp2:eval(#set{key=foo, value=bar, next=nil})),
    ?assertEqual(Expected, erlcfg_interp2:eval({set, foo, bar, nil})).

eval_set_nested_get_test()  ->
    Ast = {set, foo, bar, {set, moo, {get, foo}, nil}},
    Expected = {c, '', [{d, foo, bar}, {d, moo, bar}]},
    ?assertEqual(Expected, erlcfg_interp2:eval(Ast)).

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

    {Interp, '', Value1} = erlcfg_interp:eval(Interp, '', {cons, 1, nil}),
    ?assertEqual(Value1, [1]),

    {Interp, '', Value2} = erlcfg_interp:eval(Interp, '', {cons, 2, {cons, 1, nil}}),
    ?assertEqual(Value2, [2,1]).

eval_set_no_parent_test()  ->
    Ast = {set, foo, bar, {set, foo.foo.bar, bar, nil}},
    Expected = {not_found, foo.foo},
    ?assertThrow(Expected, erlcfg_interp2:eval(Ast)).

eval_illegal_command_test()  ->
    Interp = erlcfg_interp:new(),
    Expected = {illegal_command, {read, moo, noop}},
    ?assertThrow(Expected, erlcfg_interp:eval(Interp, '', {read, moo, noop})).
