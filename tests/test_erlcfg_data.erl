-module(test_erlcfg_data).
-include_lib("eunit/include/eunit.hrl").


get_onelevel_single_test() ->
    Data = {c, '', [
        {d, foo, 5}
    ]},
    Config = erlcfg_data:new(Data),
    ?assertEqual(5, Config:get(foo)).

get_onelevel_dual_test() ->
    Data = {c, '', [
        {d, foo, 5}, 
        {d, bar, baz}
    ]},
    Config = erlcfg_data:new(Data),
    ?assertEqual(5, Config:get(foo)),
    ?assertEqual(baz, Config:get(bar)),
    ?assertEqual({error, {not_found, far}}, Config:get(far)).

get_onelevel_multi_test() ->
    Data = {c, '', [
        {d, int, 5}, 
        {d, atom, baz}, 
        {d, string, "A string"}
    ]},
    Config = erlcfg_data:new(Data),
    ?assertEqual(5, Config:get(int)),
    ?assertEqual(baz, Config:get(atom)), 
    ?assertEqual("A string", Config:get(string)).

get_default_value_test() ->
    Data = {c, '', [
        {d, int, 5}, 
        {d, atom, baz}, 
        {d, string, "A string"}
    ]},
    Config = erlcfg_data:new(Data),
    ?assertEqual(5, Config:get(int)),
    ?assertEqual(baz, Config:get(atom, 10)),
    ?assertEqual(10, Config:get(int.moo, 10)).


get_twolevels_test() ->
    Data = {c, '', [
        {c, foo, 
            [
                {d, int, 5}, 
                {d, float, 5.0}
            ]
        }
    ]},
    Config = erlcfg_data:new(Data),
    ?assertEqual(5, Config:get(foo.int)), 
    ?assertEqual(5.0, Config:get(foo.float)),
    ?assertEqual({error, {not_found, foo.bar}}, Config:get(foo.bar)).

get_multi_level_nested_test() ->
    Data = {c, '', [
        {c, one, 
            [ 
                {d, one, 11}, 
                {d, two, 12},
                {c, three, 
                    [ 
                        {d, one, 131}, 
                        {d, two, 132} 
                    ]
                }
            ]
        },

        {c, two, 
            [ 
                {c, one, 
                    [ 
                        {d, one, 211}, 
                        {d, two, 212} 
                    ]
                },

                {c, two, 
                    [ 
                        {d, one, 221}, 
                        {d, two, 222} 
                    ]
                }
            ]
        }


    ]},

    Config = erlcfg_data:new(Data),

    ?assertEqual(11, Config:get(one.one)), 
    ?assertEqual(12, Config:get(one.two)),
    ?assertEqual(131, Config:get(one.three.one)),
    ?assertEqual(132, Config:get(one.three.two)),

    ?assertEqual(211, Config:get(two.one.one)),
    ?assertEqual(212, Config:get(two.one.two)),
    ?assertEqual(221, Config:get(two.two.one)),
    ?assertEqual(222, Config:get(two.two.two)).
