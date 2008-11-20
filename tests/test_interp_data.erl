-module(test_interp_data).
-include_lib("eunit/include/eunit.hrl").

find_empty_test() ->
    ?assertEqual({node, bar}, interp_data:find([], [foo], bar)).

find_onelevel_single_test() ->
    Data = [
        {foo, 5}
    ],
    ?assertEqual({node, 5}, interp_data:find([foo], [], Data)).

find_onelevel_dual_test() ->
    Data = [
        {foo, 5}, 
        {bar, baz}
    ],
    ?assertEqual({node, 5}, interp_data:find([foo], [], Data)),
    ?assertEqual({node, baz}, interp_data:find([bar], [], Data)).

find_onelevel_multi_test() ->
    Data = [
        {int, 5}, 
        {atom, baz}, 
        {string, "A string"}
    ],
    ?assertEqual({node, 5}, interp_data:find([int], [], Data)),
    ?assertEqual({node, baz}, interp_data:find([atom], [], Data)),
    ?assertEqual({node, "A string"}, interp_data:find([string], [], Data)).


find_twolevels_test() ->
    Data = [
        {foo, 
            [
                {int, 5}, 
                {float, 5.0}
            ]
        }
    ],
    ?assertEqual({node, 5}, interp_data:find([foo, int], [], Data)),
    ?assertEqual({node, 5.0}, interp_data:find([foo, float], [], Data)).

find_multi_level_nested_test() ->
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

    ?assertEqual({node, 11}, interp_data:find([one, one], [], Data)),
    ?assertEqual({node, 12}, interp_data:find([one, two], [], Data)),
    ?assertEqual({node, 131}, interp_data:find([one, three, one], [], Data)),
    ?assertEqual({node, 132}, interp_data:find([one, three, two], [], Data)),

    ?assertEqual({node, 211}, interp_data:find([two, one, one], [], Data)),
    ?assertEqual({node, 212}, interp_data:find([two, one, two], [], Data)),
    ?assertEqual({node, 221}, interp_data:find([two, two, one], [], Data)),
    ?assertEqual({node, 222}, interp_data:find([two, two, two], [], Data)).




find2_onelevel_single_test() ->
    Data = [
        {foo, 5}
    ],
    ?assertEqual({node, 5}, interp_data:find(Data, foo)).

find2_onelevel_dual_test() ->
    Data = [
        {foo, 5}, 
        {bar, baz}
    ],
    ?assertEqual({node, 5}, interp_data:find(Data, foo)),
    ?assertEqual({node, baz}, interp_data:find(Data, bar)).

find2_onelevel_multi_test() ->
    Data = [
        {int, 5}, 
        {atom, baz}, 
        {string, "A string"}
    ],
    ?assertEqual({node, 5}, interp_data:find(Data, int)),
    ?assertEqual({node, baz}, interp_data:find(Data, atom)),
    ?assertEqual({node, "A string"}, interp_data:find(Data, string)).


find2_twolevels_test() ->
    Data = [
        {foo, 
            [
                {int, 5}, 
                {float, 5.0}
            ]
        }
    ],
    ?assertEqual({node, 5}, interp_data:find(Data, foo.int)),
    ?assertEqual({node, 5.0}, interp_data:find(Data, foo.float)).

find2_multi_level_nested_test() ->
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

    ?assertEqual({node, 11}, interp_data:find(Data, one.one)),
    ?assertEqual({node, 12}, interp_data:find(Data, one.two)),
    ?assertEqual({node, 131}, interp_data:find(Data, one.three.one)),
    ?assertEqual({node, 132}, interp_data:find(Data, one.three.two)),

    ?assertEqual({node, 211}, interp_data:find(Data, two.one.one)),
    ?assertEqual({node, 212}, interp_data:find(Data, two.one.two)),
    ?assertEqual({node, 221}, interp_data:find(Data, two.two.one)),
    ?assertEqual({node, 222}, interp_data:find(Data, two.two.two)).
