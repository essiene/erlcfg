-module(test_erlcfg_interp).
-include_lib("eunit/include/eunit.hrl").
-include("erlcfg.hrl").


eval_nil_test() ->
    Expected = {interp, {c, '', []}, nil, ''},
    ?assertEqual(Expected, erlcfg_interp2:eval(nil)).

eval_set_test()  ->
    Expected = {interp, {c, '', [{d, foo, bar}]}, bar, ''},
    ?assertEqual(Expected, erlcfg_interp2:eval(#set{key=foo, value=bar, next=nil})),
    ?assertEqual(Expected, erlcfg_interp2:eval({set, foo, bar, nil})).

eval_set_nested_get_test()  ->
    Ast = {set, foo, bar, {set, moo, {get, foo}, nil}},
    Expected = {interp, {c, '', [{d, foo, bar}, {d, moo, bar}]}, bar, ''},
    ?assertEqual(Expected, erlcfg_interp2:eval(Ast)).

eval_block_test() ->
    Ast = {block, foo, nil, nil},
    Expected = {interp, {c, '', [{c, foo, []}]}, nil, ''},
    ?assertEqual(Expected, erlcfg_interp2:eval(Ast)).

eval_nested_blocks_test() ->
    Ast = {block, foo, {set, foo, bar, {block, foo1, {set, foo, bar, nil}, nil}}, {set, foo1, bar1, nil}},
    Expected = {interp, {c, '', [{c, foo, [{d, foo, bar}, {c, foo1, [{d, foo, bar}]}]}, {d, foo1, bar1}]}, bar1, ''},
    ?assertEqual(Expected, erlcfg_interp2:eval(Ast)).

eval_integer_test() ->
    Ast = {set, foo, 5, nil},
    Expected = {interp, {c, '', [{d, foo, 5}]}, 5, ''},
    ?assertEqual(Expected, erlcfg_interp2:eval(Ast)).

eval_float_test() ->
    Ast = {set, foo, -5.5e-15, nil},
    Expected = {interp, {c, '', [{d, foo, -5.5e-15}]}, -5.5e-15, ''},
    ?assertEqual(Expected, erlcfg_interp2:eval(Ast)).

eval_atom_test() ->
    Ast = {set, foo, a95, nil},
    Expected = {interp, {c, '', [{d, foo, a95}]}, a95, ''},
    ?assertEqual(Expected, erlcfg_interp2:eval(Ast)).

eval_string_test() ->
    Ast = {set, foo, <<"A String">>, nil},
    Expected = {interp, {c, '', [{d, foo, "A String"}]}, "A String", ''},
    ?assertEqual(Expected, erlcfg_interp2:eval(Ast)).

eval_cons_empty_test() ->
    Ast = [],
    Expected = {interp, {c, '', []}, [], ''},
    ?assertEqual(Expected, erlcfg_interp2:eval(Ast)).

eval_cons_single_item_test() ->
    Ast = {cons, 1, []},
    Expected = {interp, {c, '', []}, [1], ''},
    ?assertEqual(Expected, erlcfg_interp2:eval(Ast)).

eval_cons_multiple_items_test() ->
    Ast = {cons, 1, {cons, 2, {cons, 3, []}}},
    Expected = {interp, {c, '', []}, [1,2,3], ''},
    ?assertEqual(Expected, erlcfg_interp2:eval(Ast)).

eval_set_no_parent_test()  ->
    Ast = {set, foo, bar, {set, foo.foo.bar, bar, nil}},
    Expected = {not_found, foo.foo},
    ?assertThrow(Expected, erlcfg_interp2:eval(Ast)).

eval_illegal_command_test()  ->
    Expected = {illegal_command, {read, moo, noop}},
    ?assertThrow(Expected, erlcfg_interp2:eval({read, moo, noop})).
