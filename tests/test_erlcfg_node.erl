-module(test_erlcfg_node).
-include_lib("eunit/include/eunit.hrl").

node_find_empty_test() ->
    ?assertEqual({node, bar}, erlcfg_node:node_find([], [foo], bar)).

node_find_onelevel_single_test() ->
    Data = [
        {foo, 5}
    ],
    ?assertEqual({node, 5}, erlcfg_node:node_find([foo], [], Data)).

node_find_onelevel_dual_test() ->
    Data = [
        {foo, 5}, 
        {bar, baz}
    ],
    ?assertEqual({node, 5}, erlcfg_node:node_find([foo], [], Data)),
    ?assertEqual({node, baz}, erlcfg_node:node_find([bar], [], Data)).

node_find_onelevel_multi_test() ->
    Data = [
        {int, 5}, 
        {atom, baz}, 
        {string, "A string"}
    ],
    ?assertEqual({node, 5}, erlcfg_node:node_find([int], [], Data)),
    ?assertEqual({node, baz}, erlcfg_node:node_find([atom], [], Data)),
    ?assertEqual({node, "A string"}, erlcfg_node:node_find([string], [], Data)).


node_find_twolevels_test() ->
    Data = [
        {foo, 
            [
                {int, 5}, 
                {float, 5.0}
            ]
        }
    ],
    ?assertEqual({node, 5}, erlcfg_node:node_find([foo, int], [], Data)),
    ?assertEqual({node, 5.0}, erlcfg_node:node_find([foo, float], [], Data)).

node_find_multi_level_nested_test() ->
    Data = [
        {one, 
            [ 
                {one, 11}, 
                {two, 12},
                {three, 
                    [ 
                        {one, 131}, 
                        {two, 132} 
                    ]
                }
            ]
        },

        {two, 
            [ 
                {one, 
                    [ 
                        {one, 211}, 
                        {two, 212} 
                    ]
                },

                {two, 
                    [ 
                        {one, 221}, 
                        {two, 222} 
                    ]
                }
            ]
        }


    ],

    ?assertEqual({node, 11}, erlcfg_node:node_find([one, one], [], Data)),
    ?assertEqual({node, 12}, erlcfg_node:node_find([one, two], [], Data)),
    ?assertEqual({node, 131}, erlcfg_node:node_find([one, three, one], [], Data)),
    ?assertEqual({node, 132}, erlcfg_node:node_find([one, three, two], [], Data)),

    ?assertEqual({node, 211}, erlcfg_node:node_find([two, one, one], [], Data)),
    ?assertEqual({node, 212}, erlcfg_node:node_find([two, one, two], [], Data)),
    ?assertEqual({node, 221}, erlcfg_node:node_find([two, two, one], [], Data)),
    ?assertEqual({node, 222}, erlcfg_node:node_find([two, two, two], [], Data)).




node_find2_onelevel_single_test() ->
    Data = [
        {foo, 5}
    ],
    ?assertEqual({node, 5}, erlcfg_node:node_find(Data, foo)).

node_find2_onelevel_dual_test() ->
    Data = [
        {foo, 5}, 
        {bar, baz}
    ],
    ?assertEqual({node, 5}, erlcfg_node:node_find(Data, foo)),
    ?assertEqual({node, baz}, erlcfg_node:node_find(Data, bar)).

node_find2_onelevel_multi_test() ->
    Data = [
        {int, 5}, 
        {atom, baz}, 
        {string, "A string"}
    ],
    ?assertEqual({node, 5}, erlcfg_node:node_find(Data, int)),
    ?assertEqual({node, baz}, erlcfg_node:node_find(Data, atom)),
    ?assertEqual({node, "A string"}, erlcfg_node:node_find(Data, string)).


node_find2_twolevels_test() ->
    Data = [
        {foo, 
            [
                {int, 5}, 
                {float, 5.0}
            ]
        }
    ],
    ?assertEqual({node, 5}, erlcfg_node:node_find(Data, foo.int)),
    ?assertEqual({node, 5.0}, erlcfg_node:node_find(Data, foo.float)).

node_find2_multi_level_nested_test() ->
    Data = [
        {one, 
            [ 
                {one, 11}, 
                {two, 12},
                {three, 
                    [ 
                        {one, 131}, 
                        {two, 132} 
                    ]
                }
            ]
        },

        {two, 
            [ 
                {one, 
                    [ 
                        {one, 211}, 
                        {two, 212} 
                    ]
                },

                {two, 
                    [ 
                        {one, 221}, 
                        {two, 222} 
                    ]
                }
            ]
        }


    ],

    ?assertEqual({node, 11}, erlcfg_node:node_find(Data, one.one)),
    ?assertEqual({node, 12}, erlcfg_node:node_find(Data, one.two)),
    ?assertEqual({node, 131}, erlcfg_node:node_find(Data, one.three.one)),
    ?assertEqual({node, 132}, erlcfg_node:node_find(Data, one.three.two)),

    ?assertEqual({node, 211}, erlcfg_node:node_find(Data, two.one.one)),
    ?assertEqual({node, 212}, erlcfg_node:node_find(Data, two.one.two)),
    ?assertEqual({node, 221}, erlcfg_node:node_find(Data, two.two.one)),
    ?assertEqual({node, 222}, erlcfg_node:node_find(Data, two.two.two)).
