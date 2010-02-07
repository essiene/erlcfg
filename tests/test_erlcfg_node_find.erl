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

-module(test_erlcfg_node_find).
-include_lib("eunit/include/eunit.hrl").

node_find_empty_test() ->
    ?assertEqual({d, foo, bar}, erlcfg_node:node_find([], [foo], {d, foo, bar})).

node_find_onelevel_single_test() ->
    Data = {c, '', [
        {d, foo, 5}
    ]},
    ?assertEqual({d, foo, 5}, erlcfg_node:node_find([foo], [], Data)).

node_find_onelevel_dual_test() ->
    Data = {c, '', [
        {d, foo, 5}, 
        {d, bar, baz}
    ]},
    ?assertEqual({d, foo, 5}, erlcfg_node:node_find([foo], [], Data)),
    ?assertEqual({d, bar, baz}, erlcfg_node:node_find([bar], [], Data)).

node_find_onelevel_multi_test() ->
    Data = {c, '', [
        {d, int, 5}, 
        {d, atom, baz}, 
        {d, string, "A string"}
    ]},
    ?assertEqual({d, int, 5}, erlcfg_node:node_find([int], [], Data)),
    ?assertEqual({d, atom, baz}, erlcfg_node:node_find([atom], [], Data)),
    ?assertEqual({d, string, "A string"}, erlcfg_node:node_find([string], [], Data)).


node_find_twolevels_test() ->
    Data = {c, '', [
        {c, foo, 
            [
                {d, int, 5}, 
                {d, float, 5.0}
            ]
        }
    ]},
    ?assertEqual({d, int, 5}, erlcfg_node:node_find([foo, int], [], Data)),
    ?assertEqual({d, float, 5.0}, erlcfg_node:node_find([foo, float], [], Data)).

node_find_multi_level_nested_test() ->
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

    ?assertEqual({d, one, 11}, erlcfg_node:node_find([one, one], [], Data)),
    ?assertEqual({d, two, 12}, erlcfg_node:node_find([one, two], [], Data)),
    ?assertEqual({d, one, 131}, erlcfg_node:node_find([one, three, one], [], Data)),
    ?assertEqual({d, two, 132}, erlcfg_node:node_find([one, three, two], [], Data)),

    ?assertEqual({d, one, 211}, erlcfg_node:node_find([two, one, one], [], Data)),
    ?assertEqual({d, two, 212}, erlcfg_node:node_find([two, one, two], [], Data)),
    ?assertEqual({d, one, 221}, erlcfg_node:node_find([two, two, one], [], Data)),
    ?assertEqual({d, two, 222}, erlcfg_node:node_find([two, two, two], [], Data)).




node_find2_onelevel_single_test() ->
    Data = {c, '', [
        {d, foo, 5}
    ]},
    ?assertEqual({d, foo, 5}, erlcfg_node:node_find(Data, foo)).

node_find2_onelevel_dual_test() ->
    Data = {c, '',[
        {d, foo, 5}, 
        {d, bar, baz}
    ]},
    ?assertEqual({d, foo, 5}, erlcfg_node:node_find(Data, foo)),
    ?assertEqual({d, bar, baz}, erlcfg_node:node_find(Data, bar)).

node_find2_onelevel_multi_test() ->
    Data = {c, '', [
        {d, int, 5}, 
        {d, atom, baz}, 
        {d, string, "A string"}
    ]},
    ?assertEqual({d, int, 5}, erlcfg_node:node_find(Data, int)),
    ?assertEqual({d, atom, baz}, erlcfg_node:node_find(Data, atom)),
    ?assertEqual({d, string, "A string"}, erlcfg_node:node_find(Data, string)).


node_find2_twolevels_test() ->
    Data = {c, '', [
        {c, foo, 
            [
                {d, int, 5}, 
                {d, float, 5.0}
            ]
        }
    ]},
    ?assertEqual({d, int, 5}, erlcfg_node:node_find(Data, foo.int)),
    ?assertEqual({d, float, 5.0}, erlcfg_node:node_find(Data, foo.float)).

node_find2_multi_level_nested_test() ->
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

    ?assertEqual({d, one, 11}, erlcfg_node:node_find(Data, one.one)),
    ?assertEqual({d, two, 12}, erlcfg_node:node_find(Data, one.two)),
    ?assertEqual({d, one, 131}, erlcfg_node:node_find(Data, one.three.one)),
    ?assertEqual({d, two, 132}, erlcfg_node:node_find(Data, one.three.two)),

    ?assertEqual({d, one, 211}, erlcfg_node:node_find(Data, two.one.one)),
    ?assertEqual({d, two, 212}, erlcfg_node:node_find(Data, two.one.two)),
    ?assertEqual({d, one, 221}, erlcfg_node:node_find(Data, two.two.one)),
    ?assertEqual({d, two, 222}, erlcfg_node:node_find(Data, two.two.two)).
