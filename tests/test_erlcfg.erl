-module(test_erlcfg).
-include_lib("eunit/include/eunit.hrl").


get_empty_test() ->
    ?assertEqual({value, {["foo"], bar}}, erlcfg:get([], ["foo"], bar)).

get_onelevel_single_test() ->
    Data = [
        {"foo", 5}
    ],
    ?assertEqual({value, {["foo"], 5}}, erlcfg:get(["foo"], [], Data)).

get_onelevel_dual_test() ->
    Data = [
        {"foo", 5}, 
        {"bar", baz}
    ],
    ?assertEqual({value, {["foo"], 5}}, erlcfg:get(["foo"], [], Data)),
    ?assertEqual({value, {["bar"], baz}}, erlcfg:get(["bar"], [], Data)).

get_onelevel_multi_test() ->
    Data = [
        {"int", 5}, 
        {"atom", baz}, 
        {"string", "A string"}
    ],
    ?assertEqual({value, {["int"], 5}}, erlcfg:get(["int"], [], Data)),
    ?assertEqual({value, {["atom"], baz}}, erlcfg:get(["atom"], [], Data)),
    ?assertEqual({value, {["string"], "A string"}}, erlcfg:get(["string"], [], Data)).


get_twolevels_test() ->
    Data = [
        {"foo", 
            [
                {"int", 5}, 
                {"float", 5.0}
            ]
        }
    ],
    ?assertEqual({value, {["foo", "int"], 5}}, erlcfg:get(["foo", "int"], [], Data)),
    ?assertEqual({value, {["foo", "float"], 5.0}}, erlcfg:get(["foo", "float"], [], Data)).

get_multi_level_nested_test() ->
    Data = [
        {"one", 
            [ 
                {"one", 11}, 
                {"two", 12},
                {"three", 
                    [ 
                        {"one", 131}, 
                        {"two", 132} 
                    ]
                }
            ]
        },

        {"two", 
            [ 
                {"one", 
                    [ 
                        {"one", 211}, 
                        {"two", 212} 
                    ]
                },

                {"two", 
                    [ 
                        {"one", 221}, 
                        {"two", 222} 
                    ]
                }
            ]
        }


    ],

    ?assertEqual({value, {["one", "one"], 11}}, erlcfg:get(["one", "one"], [], Data)),
    ?assertEqual({value, {["one", "two"], 12}}, erlcfg:get(["one", "two"], [], Data)),
    ?assertEqual({value, {["one", "three", "one"], 131}}, erlcfg:get(["one", "three", "one"], [], Data)),
    ?assertEqual({value, {["one", "three", "two"], 132}}, erlcfg:get(["one", "three", "two"], [], Data)),

    ?assertEqual({value, {["two", "one", "one"], 211}}, erlcfg:get(["two", "one", "one"], [], Data)),
    ?assertEqual({value, {["two", "one", "two"], 212}}, erlcfg:get(["two", "one", "two"], [], Data)),
    ?assertEqual({value, {["two", "two", "one"], 221}}, erlcfg:get(["two", "two", "one"], [], Data)),
    ?assertEqual({value, {["two", "two", "two"], 222}}, erlcfg:get(["two", "two", "two"], [], Data)).
