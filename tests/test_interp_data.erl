-module(test_interp_data).
-include_lib("eunit/include/eunit.hrl").

get_empty_test() ->
    ?assertEqual({value, {[foo], bar}}, interp_data:get([], [foo], bar)).

get_onelevel_single_test() ->
    Data = [
        {foo, 5}
    ],
    ?assertEqual({value, {[foo], 5}}, interp_data:get([foo], [], Data)).

get_onelevel_dual_test() ->
    Data = [
        {foo, 5}, 
        {bar, baz}
    ],
    ?assertEqual({value, {[foo], 5}}, interp_data:get([foo], [], Data)),
    ?assertEqual({value, {[bar], baz}}, interp_data:get([bar], [], Data)).

get_onelevel_multi_test() ->
    Data = [
        {int, 5}, 
        {atom, baz}, 
        {string, "A string"}
    ],
    ?assertEqual({value, {[int], 5}}, interp_data:get([int], [], Data)),
    ?assertEqual({value, {[atom], baz}}, interp_data:get([atom], [], Data)),
    ?assertEqual({value, {[string], "A string"}}, interp_data:get([string], [], Data)).


get_twolevels_test() ->
    Data = [
        {foo, 
            [
                {int, 5}, 
                {float, 5.0}
            ]
        }
    ],
    ?assertEqual({value, {[foo, int], 5}}, interp_data:get([foo, int], [], Data)),
    ?assertEqual({value, {[foo, float], 5.0}}, interp_data:get([foo, float], [], Data)).

get_multi_level_nested_test() ->
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

    ?assertEqual({value, {[one, one], 11}}, interp_data:get([one, one], [], Data)),
    ?assertEqual({value, {[one, two], 12}}, interp_data:get([one, two], [], Data)),
    ?assertEqual({value, {[one, three, one], 131}}, interp_data:get([one, three, one], [], Data)),
    ?assertEqual({value, {[one, three, two], 132}}, interp_data:get([one, three, two], [], Data)),

    ?assertEqual({value, {[two, one, one], 211}}, interp_data:get([two, one, one], [], Data)),
    ?assertEqual({value, {[two, one, two], 212}}, interp_data:get([two, one, two], [], Data)),
    ?assertEqual({value, {[two, two, one], 221}}, interp_data:get([two, two, one], [], Data)),
    ?assertEqual({value, {[two, two, two], 222}}, interp_data:get([two, two, two], [], Data)).




get2_onelevel_single_test() ->
    Data = [
        {foo, 5}
    ],
    ?assertEqual({value, 5}, interp_data:get(Data, foo)).

get2_onelevel_dual_test() ->
    Data = [
        {foo, 5}, 
        {bar, baz}
    ],
    ?assertEqual({value, 5}, interp_data:get(Data, foo)),
    ?assertEqual({value, baz}, interp_data:get(Data, bar)).

get2_onelevel_multi_test() ->
    Data = [
        {int, 5}, 
        {atom, baz}, 
        {string, "A string"}
    ],
    ?assertEqual({value, 5}, interp_data:get(Data, int)),
    ?assertEqual({value, baz}, interp_data:get(Data, atom)),
    ?assertEqual({value, "A string"}, interp_data:get(Data, string)).


get2_twolevels_test() ->
    Data = [
        {foo, 
            [
                {int, 5}, 
                {float, 5.0}
            ]
        }
    ],
    ?assertEqual({value, 5}, interp_data:get(Data, foo.int)),
    ?assertEqual({value, 5.0}, interp_data:get(Data, foo.float)).

get2_multi_level_nested_test() ->
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

    ?assertEqual({value, 11}, interp_data:get(Data, one.one)),
    ?assertEqual({value, 12}, interp_data:get(Data, one.two)),
    ?assertEqual({value, 131}, interp_data:get(Data, one.three.one)),
    ?assertEqual({value, 132}, interp_data:get(Data, one.three.two)),

    ?assertEqual({value, 211}, interp_data:get(Data, two.one.one)),
    ?assertEqual({value, 212}, interp_data:get(Data, two.one.two)),
    ?assertEqual({value, 221}, interp_data:get(Data, two.two.one)),
    ?assertEqual({value, 222}, interp_data:get(Data, two.two.two)).
