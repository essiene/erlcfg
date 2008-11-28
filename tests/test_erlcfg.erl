-module(test_erlcfg).
-include_lib("eunit/include/eunit.hrl").

new_no_configfile_test() ->
    ?assertEqual({erlcfg_data, {c, '', []}}, erlcfg:new()).

new_test() ->
    Config = erlcfg:new("flat.conf"),
    ?assertEqual(1, Config:get(one_int)),
    ?assertEqual(10.038e-10, Config:get(two_float)),
    ?assertEqual(atom3, Config:get(three_atom)),
    ?assertEqual(awesome@quoted.atom, Config:get(four_quoted_atom)),
    ?assertEqual("A string", Config:get(five_string)).

new_variable_test() ->
    Config = erlcfg:new("flat.conf"),
    ?assertEqual(1, Config:get(six_variable)),
    ?assertEqual(awesome@quoted.atom, Config:get(seven_variable)).

nested_file_test() ->
    Config = erlcfg:new("full.conf"),
    ?assertEqual("www.appserver.com", Config:get(common.appserver)),
    ?assertEqual(5038, Config:get(common.port.ami)),
    ?assertEqual(9119, Config:get(common.port.rest)),

    ?assertEqual(["10.10.201.5", "192.168.10.41"], Config:get(general.listen)),
    ?assertEqual(9119, Config:get(general.port)),
    ?assertEqual(2, Config:get(general.wait.short)),
    ?assertEqual(10, Config:get(general.wait.long)),

    ?assertEqual("www.appserver.com", Config:get(ami.host)),
    ?assertEqual(5038, Config:get(ami.port)),
    ?assertEqual("user", Config:get(ami.username)),
    ?assertEqual("pass", Config:get(ami.secret)),

    ?assertEqual(5, Config:get(callentry.rttl)),
    ?assertEqual(60, Config:get(callentry.qttl)),
    ?assertEqual(high, Config:get(callentry.requeue.priority)).

