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
