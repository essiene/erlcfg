-module(test_erlcfg_node).
-include_lib("eunit/include/eunit.hrl").


node_empty_set_test() ->
    Node = [],
    Expected = [
        {foo, bar}
    ],
    Result = erlcfg_node:node_set(Node, foo, bar),
    ?assertEqual(Expected, Result).

node_singlecontent_set_test() ->
    Node = [
        {bar, baz}
    ],
    Expected = [
        {bar, baz},
        {foo, bar}
    ],
    Result = erlcfg_node:node_set(Node, foo, bar),
    ?assertEqual(Expected, Result).

node_multiplecontent_set_test() ->
    Node = [
        {bar, baz},
        {moo, foo}
    ],
    Expected = [
        {bar, baz},
        {moo, foo},
        {foo, bar}
    ],
    Result = erlcfg_node:node_set(Node, foo, bar),
    ?assertEqual(Expected, Result).

node_multiplecontent_multipleset_collision_test() ->
    Node = [
        {bar, baz},
        {moo, foo}
    ],

    Expected = [
        {bar, baz},
        {moo, foo},
        {foo, bar}
    ],
    Result = erlcfg_node:node_set(Node, foo, bar),
    ?assertEqual(Expected, Result),

    Expected1 = [
        {bar, foo},
        {moo, foo},
        {foo, bar}
    ],

    Result1 = erlcfg_node:node_set(Result, bar, foo),
    ?assertEqual(Expected1, Result1).

node_get_when_empty_test() ->
    Node = [],
    Expected = {error, undefined},
    Result = erlcfg_node:node_get(Node, foo),
    ?assertEqual(Expected, Result).

node_get_when_not_empty_and_key_not_set_test() ->
    Node = [
        {foo, bar}
    ],
    Expected = {error, undefined},
    Result = erlcfg_node:node_get(Node, moo),
    ?assertEqual(Expected, Result).

node_get_single_entry_and_key_is_set_test() ->
    Node = [
        {foo, bar}
    ],
    Expected = {value, bar},
    Result = erlcfg_node:node_get(Node, foo),
    ?assertEqual(Expected, Result).

node_get_multiple_entry_and_key_is_set_test() ->
    Node = [
        {foo, bar}
    ],
    Expected = {value, bar},
    Result = erlcfg_node:node_get(Node, foo),
    ?assertEqual(Expected, Result).

node_get_multiple_entry_and_multiple_get_test() ->
    Node = [
        {foo, bar},
        {foo1, bar1},
        {foo2, bar2}
    ],

    Expected = {value, bar},
    Result = erlcfg_node:node_get(Node, foo),
    ?assertEqual(Expected, Result),

    Expected1 = {value, bar1},
    Result1 = erlcfg_node:node_get(Node, foo1),
    ?assertEqual(Expected1, Result1),

    Expected2 = {value, bar2},
    Result2 = erlcfg_node:node_get(Node, foo2),
    ?assertEqual(Expected2, Result2),

    Expected3 = {error, undefined},
    Result3 = erlcfg_node:node_get(Node, foo3),
    ?assertEqual(Expected3, Result3).


