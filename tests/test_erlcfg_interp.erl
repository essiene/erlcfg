-module(test_erlcfg_interp).
-include_lib("eunit/include/eunit.hrl").
-include("erlcfg.hrl").


interp_set_test()  ->
    ?assertMatch(
        {ok, #interp{node={c, '', [{d, foo, bar}]}, value=bar, schema_table=nil, macros=[]}},
        erlcfg_interp:interpret([#set{key=foo, value=bar}])),
    ?assertMatch(
        {ok, #interp{node={c, '', [{d, foo, bar}]}, value=bar, schema_table=nil, macros=[]}},
        erlcfg_interp:interpret([{set, foo, bar}])).

interp_set_nested_get_test()  ->
    AstList = [
        {set, foo, bar}, 
        {set, moo, {get, foo}}
    ],
    ?assertMatch(
        {ok, #interp{node={c, '', [{d, foo, bar}, {d, moo, bar}]}, value=bar}},
        erlcfg_interp:interpret(AstList)).

interp_empty_block_test() ->
    AstList = [{block, foo, []}],
    ?assertMatch(
        {ok, #interp{node={c, '', [{c, foo, []}]}, value=nil}},
        erlcfg_interp:interpret(AstList)).

interp_non_empty_block_test() ->
    AstList = [{block, foo, [
                {set, foo, bar},
                {set, bar, baz}
            ]}],
    ?assertMatch(
        {ok, #interp{node={c, '', [{c, foo, [{d, foo, bar}, {d, bar, baz}]}]},
                     value=baz}},
        erlcfg_interp:interpret(AstList)).

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
    ?assertMatch(
        {ok, #interp{
            node={c, '', [
                    {c, foo, [
                            {d, foo, bar}, 
                            {c, foo1, [
                                    {d, foo, bar},
                                    {d, foo1, bar1}
                            ]},
                            {d, foo2, bar2}
                    ]}
            ]}, 
            value=bar2}},
        erlcfg_interp:interpret(AstList)).

interp_integer_test() ->
    AstList = [{set, foo, 5}],
    ?assertMatch(
        {ok, #interp{node={c, '', [{d, foo, 5}]}, value=5}},
        erlcfg_interp:interpret(AstList)).

interp_float_test() ->
    AstList = [{set, foo, -5.5e-15}],
    ?assertMatch(
        {ok, #interp{node={c, '', [{d, foo, -5.5e-15}]}, value=-5.5e-15}},
        erlcfg_interp:interpret(AstList)).

interp_atom_test() ->
    AstList = [{set, foo, a95}],
    ?assertMatch(
        {ok, #interp{node={c, '', [{d, foo, a95}]}, value=a95}},
        erlcfg_interp:interpret(AstList)).

interp_string_test() ->
    AstList = [{set, foo, <<"A String">>}],
    ?assertMatch(
        {ok, #interp{node={c, '', [{d, foo, <<"A String">>}]}, value = <<"A String">>}},
        erlcfg_interp:interpret(AstList)).

interp_cons_empty_test() ->
    AstList = [],
    ?assertMatch(
        {ok, #interp{node={c, '', []}, value=nil}},
        erlcfg_interp:interpret(AstList)).

interp_cons_single_item_test() ->
    AstList = [{set, foo, {list, {cons, 1, nil}}}],
    ?assertMatch(
        {ok, #interp{node={c, '', [{d, foo, [1]}]}, value=[1]}},
        erlcfg_interp:interpret(AstList)).

interp_cons_multiple_items_test() ->
    AstList = [{set, foo, {list, {cons, 1, {cons, 2, {cons, 3, nil}}}}}],
    ?assertMatch(
        {ok, #interp{node={c, '', [{d, foo, [1,2,3]}]}, value=[1,2,3]}},
        erlcfg_interp:interpret(AstList)).

interp_set_no_parent_test()  ->
    AstList = [{set, foo, bar}, 
        {set, 'foo.foo.bar', bar}],
    Expected = {not_found, 'foo.foo'},
    ?assertThrow(Expected, erlcfg_interp:interpret(AstList)).

interp_illegal_command_test()  ->
    Expected = {unsupported_value_type, {read, moo, noop}},
    ?assertThrow(Expected, erlcfg_interp:interpret([{set, foo, {read, moo, noop}}])).
