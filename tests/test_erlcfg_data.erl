-module(test_erlcfg_data).
-include_lib("eunit/include/eunit.hrl").


get_onelevel_single_test() ->
    Data = {c, '', [
        {d, foo, 5}
    ]},
    Config = erlcfg_data:new(Data),
    ?assertEqual({value, 5}, Config:get(foo)).

get_onelevel_dual_test() ->
    Data = {c, '', [
        {d, foo, 5}, 
        {d, bar, baz}
    ]},
    Config = erlcfg_data:new(Data),
    ?assertEqual({value, 5}, Config:get(foo)),
    ?assertEqual({value, baz}, Config:get(bar)).

get_onelevel_multi_test() ->
    Data = {c, '', [
        {d, int, 5}, 
        {d, atom, baz}, 
        {d, string, <<"A string">>}
    ]},
    Config = erlcfg_data:new(Data),
    ?assertEqual({value, 5}, Config:get(int)),
    ?assertEqual({value, baz}, Config:get(atom)), 
    ?assertEqual({value, "A string"}, Config:get(string)).


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
    ?assertEqual({value, 5}, Config:get(foo.int)), 
    ?assertEqual({value, 5.0}, Config:get(foo.float)).

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

    ?assertEqual({value, 11}, Config:get(one.one)), 
    ?assertEqual({value, 12}, Config:get(one.two)),
    ?assertEqual({value, 131}, Config:get(one.three.one)),
    ?assertEqual({value, 132}, Config:get(one.three.two)),

    ?assertEqual({value, 211}, Config:get(two.one.one)),
    ?assertEqual({value, 212}, Config:get(two.one.two)),
    ?assertEqual({value, 221}, Config:get(two.two.one)),
    ?assertEqual({value, 222}, Config:get(two.two.two)).
