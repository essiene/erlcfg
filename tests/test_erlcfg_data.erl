%% 
%% Copyright (c) 2008-2010, Essien Ita Essien
%% All rights reserved.
%% 
%% Redistribution and use in source and binary forms, 
%% with or without modification, are permitted 
%% provided that the following conditions are met:
%%
%%    * Redistributions of source code must retain the 
%%      above copyright notice, this list of conditions 
%%      and the following disclaimer.
%%    * Redistributions in binary form must reproduce 
%%      the above copyright notice, this list of 
%%      conditions and the following disclaimer in the 
%%      documentation and/or other materials provided with 
%%      the distribution.
%%    * Neither the name "JsonEvents" nor the names of its 
%%      contributors may be used to endorse or promote 
%%      products derived from this software without 
%%      specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND 
%% CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED 
%% WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
%% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR 
%% PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT 
%% HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, 
%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES 
%% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE 
%% GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR 
%% BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF 
%% LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT 
%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF 
%% THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY 
%% OF SUCH DAMAGE.
%% 

-module(test_erlcfg_data).
-include_lib("eunit/include/eunit.hrl").


get_onelevel_single_test() ->
    Data = {c, '', [
        {d, foo, 5}
    ]},
    Config = erlcfg_data:new(Data),
    ?assertEqual(5, Config:get(foo)).

set_onelevel_single_test() ->
    Data = {c, '', [
        {d, foo, 5}
    ]},
    Config = erlcfg_data:new(Data),
    Config1 = Config:set(foo, 10),
    ?assertEqual(10, Config1:get(foo)).

get_onelevel_dual_test() ->
    Data = {c, '', [
        {d, foo, 5}, 
        {d, bar, baz}
    ]},
    Config = erlcfg_data:new(Data),
    ?assertEqual(5, Config:get(foo)),
    ?assertEqual(baz, Config:get(bar)),
    ?assertEqual({error, {not_found, far}}, Config:get(far)).

set_onelevel_dual_test() ->
    Data = {c, '', [
        {d, foo, 5}, 
        {d, bar, baz}
    ]},
    Config = erlcfg_data:new(Data),
    Config1 = Config:set(foo, 10),
    Config2 = Config1:set(bar, fiz),
    Config3 = Config2:set(far, moo),
    ?assertEqual(10, Config3:get(foo)),
    ?assertEqual(fiz, Config3:get(bar)),
    ?assertEqual(moo, Config3:get(far)),
    ?assertEqual({error, {not_found, too}}, Config3:set(too.far, yes)).

create_test() ->
    Data = {c, '', [
        {d, foo, 5}, 
        {d, bar, baz}
    ]},
    Config = erlcfg_data:new(Data),
    Config1 = Config:create(moo.foo.doo, fiz),
    ?assertEqual(fiz, Config1:get(moo.foo.doo)).

get_onelevel_multi_test() ->
    Data = {c, '', [
        {d, int, 5}, 
        {d, atom, baz}, 
        {d, string, <<"A string">>}
    ]},
    Config = erlcfg_data:new(Data),
    ?assertEqual(5, Config:get(int)),
    ?assertEqual(baz, Config:get(atom)), 
    ?assertEqual(<<"A string">>, Config:raw_get(string)),
    ?assertEqual("A string", Config:get(string)).

get_default_value_test() ->
    Data = {c, '', [
        {d, int, 5}, 
        {d, atom, baz}, 
        {d, string, <<"A string">>}
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
