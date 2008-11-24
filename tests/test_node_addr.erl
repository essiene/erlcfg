-module(test_node_addr).
-include_lib("eunit/include/eunit.hrl").


basename_test() ->
    ?assertEqual(baz, node_addr:basename(foo.bar.baz)),
    ?assertEqual(bar, node_addr:basename(foo.bar)),
    ?assertEqual(foo, node_addr:basename(foo)).

parent_test() ->
    ?assertEqual(foo.bar, node_addr:parent(foo.bar.baz)),
    ?assertEqual(foo, node_addr:parent(foo.bar)),
    ?assertEqual('', node_addr:parent(foo)).

join_test() ->
    ?assertEqual('', node_addr:join([])),
    ?assertEqual(foo, node_addr:join([foo])),
    ?assertEqual(foo.bar, node_addr:join([foo, bar])),
    ?assertEqual(foo.bar.baz, node_addr:join([foo, bar, baz])).


split_test() ->
    ?assertEqual([], node_addr:split('')),
    ?assertEqual(['', foo], node_addr:split(foo)),
    ?assertEqual([foo, bar], node_addr:split(foo.bar)),
    ?assertEqual([foo, bar, baz], node_addr:split(foo.bar.baz)).
