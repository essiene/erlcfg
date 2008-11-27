-module(test_erlcfg_node).
-include_lib("eunit/include/eunit.hrl").
-export([
        node_found_mfa/1
    ]).


node_new_test() ->
    ?assertEqual({c, '', []}, erlcfg_node:new()).

node_write_test() ->
    Node = {c, '', []},
    Expected = {c, '', [
        {d, one, 1},
        {c, two, []}
    ]},
    R1 = erlcfg_node:node_write(Node, one, 1),
    Result = erlcfg_node:node_write(R1, two),
    ?assertEqual(Expected, Result),

    Expected1 = {c, '', [
        {d, one, 1},
        {c, two, []}, 
        {d, three, 3}
    ]},

    Result1 = erlcfg_node:node_write(Result, three, 3),
    ?assertEqual(Expected1, Result1).


node_singlecontent_add_test() ->
    Node = {c, '', [
        {d, bar, baz}
    ]},

    Expected = {c, '', [
        {d, bar, baz},
        {d, foo, bar}
    ]},
    Result = erlcfg_node:node_write(Node, foo, bar),
    ?assertEqual(Expected, Result).

node_multiplecontent_multipleadd_collision_test() ->
    Node = {c, '', [
        {d, bar, baz},
        {d, moo, foo}
    ]},

    Expected = {c, '', [
        {d, bar, baz},
        {d, moo, foo},
        {d, foo, bar}
    ]},

    Result = erlcfg_node:node_write(Node, foo, bar),
    ?assertEqual(Expected, Result),

    Expected1 = {c, '', [
        {d, bar, foo},
        {d, moo, foo},
        {d, foo, bar}
    ]},

    Result1 = erlcfg_node:node_write(Result, bar, foo),
    ?assertEqual(Expected1, Result1).

node_write_child_to_data_node_test() ->
    Node = {d, bar, baz},

    Expected = {error, data_node_write_child},
    Result = erlcfg_node:node_write(Node, bar, foo),
    ?assertEqual(Expected, Result),

    Result1 = erlcfg_node:node_write(Node, bar, faa),
    ?assertEqual(Expected, Result1).

node_write_to_data_node_test() ->
    Node = {d, bar, baz},

    Expected = {d, bar, bar},
    Result = erlcfg_node:node_write(Node, bar),
    ?assertEqual(Expected, Result),

    Result1 = erlcfg_node:node_write(Node, bar),
    ?assertEqual(Expected, Result1).

node_read_when_empty_test() ->
    Node = {c, '', []},
    Expected = {error, undefined},
    Result = erlcfg_node:node_read(Node, foo),
    ?assertEqual(Expected, Result).

node_read_when_not_empty_and_key_not_set_test() ->
    Node = {c, '', [
        {d, foo, bar}
    ]},

    Expected = {error, undefined},
    Result = erlcfg_node:node_read(Node, moo),
    ?assertEqual(Expected, Result).

node_read_single_entry_and_key_is_set_test() ->
    Node = {c, '', [
        {d, foo, bar}
    ]},
    Expected = {value, bar},
    Result = erlcfg_node:node_read(Node, foo),
    ?assertEqual(Expected, Result).

node_read_multiple_entry_and_key_is_set_test() ->
    Node = {c, '', [
        {d, foo, bar}
    ]},
    Expected = {value, bar},
    Result = erlcfg_node:node_read(Node, foo),
    ?assertEqual(Expected, Result).

node_read_multiple_entry_and_multiple_get_test() ->
    Node = {c, '', [
        {d, foo, bar},
        {d, foo1, bar1},
        {d, foo2, bar2}
    ]},

    Expected = {value, bar},
    Result = erlcfg_node:node_read(Node, foo),
    ?assertEqual(Expected, Result),

    Expected1 = {value, bar1},
    Result1 = erlcfg_node:node_read(Node, foo1),
    ?assertEqual(Expected1, Result1),

    Expected2 = {value, bar2},
    Result2 = erlcfg_node:node_read(Node, foo2),
    ?assertEqual(Expected2, Result2),

    Expected3 = {error, undefined},
    Result3 = erlcfg_node:node_read(Node, foo3),
    ?assertEqual(Expected3, Result3).

node_child_read_from_data_node_test() ->
    Node = {d, foo, bar},

    Expected = {error, data_node_read_child},
    Result = erlcfg_node:node_read(Node, foo),
    ?assertEqual(Expected, Result).

node_read_from_data_node_test() ->
    Node = {d, foo, bar},

    Expected = {value, bar},
    Result = erlcfg_node:node_read(Node),
    ?assertEqual(Expected, Result).

node_read_from_container_node_test() ->
    Node = {c, boo, [
        {d, foo, bar}
    ]},
    Expected = {value, [{d, foo, bar}]},
    Result = erlcfg_node:node_read(Node),
    ?assertEqual(Expected, Result).


node_found_mfa(Node)  -> 
    {boo, foo, Node}.


if_node_found_ok_test() ->
    Data = {c, '', [
        {c, one, 
            [
                {c, two, 
                    [
                        {d, three, 123},
                        {d, four, 124}
                    ]
                },
                {c, three,
                    [
                        {d, three, 133},
                        {d, four, 134}
                    ]
                }
            ]
        },
        {d, two, void}
    ]},

    Fun = fun(Node)  ->
            {boo, foo, Node}
    end,

    Expected = {boo, foo, {c, two, [
                    {d, three, 123}, 
                    {d, four, 124} 
                ]
            }},

    Result = erlcfg_node:if_node_found(Data, one.two, Fun),
    ?assertEqual(Expected, Result),

    Expected1 = {boo, foo, {d, three, 123}}, 

    Result1 = erlcfg_node:if_node_found(Data, one.two.three, ?MODULE, node_found_mfa, []),
    ?assertEqual(Expected1, Result1).


if_node_found_not_found_test() ->
    Data = {c, '', [
        {c, one, 
            [
                {c, two, 
                    [
                        {d, three, 123},
                        {d, four, 124}
                    ]
                },
                {c, three,
                    [
                        {d, three, 133},
                        {d, four, 134}
                    ]
                }
            ]
        },
        {d, two, void}
    ]},
    Fun = fun(Node)  ->
            {boo, foo, Node}
    end,

    Expected = {not_found, one.two.five},
    Result = erlcfg_node:if_node_found(Data, one.two.five.six, Fun),
    ?assertEqual(Expected, Result),

    Expected1 = {not_found, one.three.three.four},
    Result1 = erlcfg_node:if_node_found(Data, one.three.three.four.five.six.seven, Fun),
    ?assertEqual(Expected1, Result1),

    Expected2 = {not_found, three},
    Result2 = erlcfg_node:if_node_found(Data, three.four.five, Fun),
    ?assertEqual(Expected2, Result2).


get_test() ->
    Data = {c, '', [
        {c, one, 
            [
                {c, two, 
                    [
                        {d, three, 123},
                        {d, four, 124}
                    ]
                },
                {c, three,
                    [
                        {d, three, 133},
                        {d, four, 134}
                    ]
                }
            ]
        },
        {d, two, void}
    ]},

    Expected = {value, 123},
    Result = erlcfg_node:get(Data, one.two.three),
    ?assertEqual(Expected, Result),

    Expected1 = {value, void},
    Result1 = erlcfg_node:get(Data, two),
    ?assertEqual(Expected1, Result1),

    Expected2 = {not_found, two.three},
    Result2 = erlcfg_node:get(Data, two.three.four.five),
    ?assertEqual(Expected2, Result2).

set_test() ->
    Data = {c, '', []},
    Expected = {c, '', [
        {c, one, []}, 
        {d, two, void}
    ]},

    D1 = erlcfg_node:set(Data, one),
    Data1 = erlcfg_node:set(D1, two, void),
    ?assertEqual(Expected, Data1),

    Expected2 = {c, '', [
        {c, one, 
            [ 
                {c, two, 
                    [ 
                        {d, three, 123}, 
                        {d, four, 124}
                    ]
                }
            ]
        }, 
        {d, two, void}
    ]},

    D2 = erlcfg_node:set(Data1, one.two),
    D3 = erlcfg_node:set(D2, one.two.three, 123),
    Data2 = erlcfg_node:set(D3, one.two.four, 124),
    ?assertEqual(Expected2, Data2).
