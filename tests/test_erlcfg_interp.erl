-module(test_erlcfg_interp).
-include_lib("eunit/include/eunit.hrl").
-include("erlcfg.hrl").


interp_set_test()  ->
    Expected = {ok, {interp, {c, '', [{d, foo, bar}]}, bar, nil}},
    ?assertEqual(Expected, erlcfg_interp:interpret([#set{key=foo, value=bar}])),
    ?assertEqual(Expected, erlcfg_interp:interpret([{set, foo, bar}])).

interp_set_nested_get_test()  ->
    AstList = [
        {set, foo, bar}, 
        {set, moo, {get, foo}}
    ],
    Expected = {ok, {interp, {c, '', [{d, foo, bar}, {d, moo, bar}]}, bar, nil}},
    ?assertEqual(Expected, erlcfg_interp:interpret(AstList)).

interp_empty_block_test() ->
    AstList = [{block, foo, []}],
    Expected = {ok, {interp, {c, '', [{c, foo, []}]}, nil, nil}},
    ?assertEqual(Expected, erlcfg_interp:interpret(AstList)).

interp_non_empty_block_test() ->
    AstList = [{block, foo, [
                {set, foo, bar},
                {set, bar, baz}
            ]}],
    Expected = {ok, {interp, {c, '', [{c, foo, [{d, foo, bar}, {d, bar, baz}]}]}, baz, nil}},
    ?assertEqual(Expected, erlcfg_interp:interpret(AstList)).

interp_nested_blocks_test() ->
    AstList = [
        {block, foo, [
                {set, foo, bar}, 
                {block, foo1, [
                        {set, foo, bar},
                        {set, foo1, bar1}
                ]}, 
                {set, foo2, bar2}
         ]}],
    Expected = {ok, {interp, 
        {c, '', [
                {c, foo, [
                        {d, foo, bar}, 
                        {c, foo1, [
                                {d, foo, bar},
                                {d, foo1, bar1}
                        ]},
                        {d, foo2, bar2}
                ]}
        ]}, 
    bar2, nil}},
    ?assertEqual(Expected, erlcfg_interp:interpret(AstList)).

interp_integer_test() ->
    AstList = [{set, foo, 5}],
    Expected = {ok, {interp, {c, '', [{d, foo, 5}]}, 5, nil}},
    ?assertEqual(Expected, erlcfg_interp:interpret(AstList)).

interp_float_test() ->
    AstList = [{set, foo, -5.5e-15}],
    Expected = {ok, {interp, {c, '', [{d, foo, -5.5e-15}]}, -5.5e-15, nil}},
    ?assertEqual(Expected, erlcfg_interp:interpret(AstList)).

interp_atom_test() ->
    AstList = [{set, foo, a95}],
    Expected = {ok, {interp, {c, '', [{d, foo, a95}]}, a95, nil}},
    ?assertEqual(Expected, erlcfg_interp:interpret(AstList)).

interp_string_test() ->
    AstList = [{set, foo, <<"A String">>}],
    Expected = {ok, {interp, {c, '', [{d, foo, <<"A String">>}]}, <<"A String">>, nil}},
    ?assertEqual(Expected, erlcfg_interp:interpret(AstList)).

interp_cons_empty_test() ->
    AstList = [],
    Expected = {ok, {interp, {c, '', []}, nil, nil}},
    ?assertEqual(Expected, erlcfg_interp:interpret(AstList)).

interp_cons_single_item_test() ->
    AstList = [{set, foo, {list, {cons, 1, nil}}}],
    Expected = {ok, {interp, {c, '', [{d, foo, [1]}]}, [1], nil}},
    ?assertEqual(Expected, erlcfg_interp:interpret(AstList)).

interp_cons_multiple_items_test() ->
    AstList = [{set, foo, {list, {cons, 1, {cons, 2, {cons, 3, nil}}}}}],
    Expected = {ok, {interp, {c, '', [{d, foo, [1,2,3]}]}, [1,2,3], nil}},
    ?assertEqual(Expected, erlcfg_interp:interpret(AstList)).

interp_set_no_parent_test()  ->
    AstList = [{set, foo, bar}, 
        {set, foo.foo.bar, bar}],
    Expected = {not_found, foo.foo},
    ?assertThrow(Expected, erlcfg_interp:interpret(AstList)).

interp_illegal_command_test()  ->
    Expected = {unsupported_value_type, {read, moo, noop}},
    ?assertThrow(Expected, erlcfg_interp:interpret([{set, foo, {read, moo, noop}}])).
