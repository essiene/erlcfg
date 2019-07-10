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

-module(test_erlcfg).
-include_lib("eunit/include/eunit.hrl").

new_no_configfile_test() ->
    ?assertEqual({ok, {erlcfg_data, {c, '', []}}}, erlcfg:new()).

new_test() ->
    {ok, Config} = erlcfg:new("flat.conf"),
    ?assertEqual(1, Config:get(one_int)),
    ?assertEqual(10.038e-10, Config:get(two_float)),
    ?assertEqual(atom3, Config:get(three_atom)),
    ?assertEqual('awesome@quoted.atom', Config:get(four_quoted_atom)),
    ?assertEqual("A string", Config:get(five_string)).

new_variable_test() ->
    {ok, Config} = erlcfg:new("flat.conf"),
    ?assertEqual(1, Config:get(six_variable)),
    ?assertEqual('awesome@quoted.atom', Config:get(seven_variable)).

unchecked_nested_file_test() ->
    {ok, Config} = erlcfg:new("unchecked.conf"),
    ?assertEqual("www.appserver.com", Config:get('common.appserver')),
    ?assertEqual(5038, Config:get('common.port.ami')),
    ?assertEqual(9119, Config:get('common.port.rest')),

    ?assertEqual(["10.10.201.5", "192.168.10.41"], Config:get('general.listen')),
    ?assertEqual(9119, Config:get('general.port')),
    ?assertEqual(2, Config:get('general.wait.short')),
    ?assertEqual(10, Config:get('general.wait.long')),

    ?assertEqual("www.appserver.com", Config:get('ami.host')),
    ?assertEqual(5038, Config:get('ami.port')),
    ?assertEqual("user", Config:get('ami.username')),
    ?assertEqual(5, Config:get('ami.secret')),

    ?assertEqual(5, Config:get('callentry.rttl')),
    ?assertEqual(60, Config:get('callentry.qttl')),
    ?assertEqual(high, Config:get('callentry.requeue.priority')).

checked_file_fail_test() ->
    Expected = {error,[{node, 'ami.secret'}, 
        {expected_type, string},
        {value, 5}]},
    ?assertEqual(Expected, erlcfg:new("unchecked.conf", true)).

checked_file_pass_test() -> 
    {ok, Config} = erlcfg:new("checked.conf", true),

    ?assertEqual("www.appserver.com", Config:get('common.appserver')),
    ?assertEqual(5038, Config:get('common.port.ami')),
    ?assertEqual(9119, Config:get('common.port.rest')),

    ?assertEqual(["10.10.201.5", "192.168.10.41"], Config:get('general.listen')),
    ?assertEqual(9119, Config:get('general.port')),
    ?assertEqual(2, Config:get('general.wait.short')),
    ?assertEqual(10, Config:get('general.wait.long')),

    ?assertEqual("www.appserver.com", Config:get('ami.host')),
    ?assertEqual(5038, Config:get('ami.port')),
    ?assertEqual("user", Config:get('ami.username')),
    ?assertEqual("pass", Config:get('ami.secret')),

    ?assertEqual(5, Config:get('callentry.rttl')),
    ?assertEqual(60, Config:get('callentry.qttl')),
    ?assertEqual(low, Config:get('callentry.requeue.priority')),

    ?assertThrow({macro_not_found, name}, erlcfg:new("checked2.conf", true, #{env => <<"test">>})),

    {ok, C} = erlcfg:new("checked2.conf", true, #{name => <<"k">>, env => <<"test">>}),
    ?assertEqual("k", C:get('ami.xxx')).

