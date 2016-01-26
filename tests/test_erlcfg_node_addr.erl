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

-module(test_erlcfg_node_addr).
-include_lib("eunit/include/eunit.hrl").


basename_test() ->
    ?assertEqual(baz,     erlcfg_node_addr:basename('foo.bar.baz')),
    ?assertEqual(bar,     erlcfg_node_addr:basename('foo.bar')),
    ?assertEqual(foo,     erlcfg_node_addr:basename(foo)),
    ?assertEqual(baz,     erlcfg_node_addr:basename([foo,bar,baz])),
    ?assertEqual(bar,     erlcfg_node_addr:basename([foo,bar])),
    ?assertEqual(foo,     erlcfg_node_addr:basename([foo])).

parent_test() ->
    ?assertEqual('foo.bar', erlcfg_node_addr:parent('foo.bar.baz')),
    ?assertEqual(foo,       erlcfg_node_addr:parent('foo.bar')),
    ?assertEqual('',        erlcfg_node_addr:parent(foo)),
    ?assertEqual([foo,bar], erlcfg_node_addr:parent([foo,bar,baz])),
    ?assertEqual([foo],     erlcfg_node_addr:parent([foo,bar])),
    ?assertEqual([],        erlcfg_node_addr:parent([foo])).

join_test() ->
    ?assertEqual('',            erlcfg_node_addr:join([])),
    ?assertEqual(foo,           erlcfg_node_addr:join([foo])),
    ?assertEqual(foo,           erlcfg_node_addr:join(['', foo])),
    ?assertEqual('foo.bar',     erlcfg_node_addr:join([foo, bar])),
    ?assertEqual('foo.bar.baz', erlcfg_node_addr:join([foo, bar, baz])).


split_test() ->
    ?assertEqual([],            erlcfg_node_addr:split('')),
    ?assertEqual([],            erlcfg_node_addr:split([])),
    ?assertEqual([foo],         erlcfg_node_addr:split(foo)),
    ?assertEqual([foo],         erlcfg_node_addr:split([foo])),
    ?assertEqual([foo,bar],     erlcfg_node_addr:split('foo.bar')),
    ?assertEqual([foo,bar,baz], erlcfg_node_addr:split('foo.bar.baz')),
    ?assertEqual([foo,bar,baz], erlcfg_node_addr:split([foo,bar,baz])).
