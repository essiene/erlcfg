%% 
%% Copyright (c) 2019 Serge Aleynikov
%% 

-module(test_erlcfg_util).
-include_lib("eunit/include/eunit.hrl").

strftime_str_test() ->
    ?assertEqual("~/file.${USER}.log.20170714-024001",
                 erlcfg_util:strftime("~/file.${USER}.log.%Y%m%d-%H%M%S", 1500000001, utc)),
    ?assertEqual("~/file.${USER}.log.20170713-224001",
                 erlcfg_util:strftime("~/file.${USER}.log.%Y%m%d-%H%M%S", 1500000001, local)).

strftime_long_str1_test() ->
    Var  = "%Y-",
    Diff = 2,
    Len  = 1020,
    Str  = Var  ++ string:copies("x", Len - length(Var)),
    Exp  = "2017-" ++ string:copies("x", Len - 5 + Diff),
    Res  = erlcfg_util:strftime(Str, 1500000001, local),
    ?assertEqual(length(Exp), length(Res)),
    ?assertEqual(Exp, Res).

strftime_long_str2_test() ->
    Len1 = 1023,
    Str1 = string:copies("x", Len1),
    Res  = erlcfg_util:strftime(Str1, 1500000001, local),
    ?assertEqual(length(Str1), length(Res)),
    ?assertEqual(Str1, Res).

strftime_long_str3_test() ->
    Len1 = 1024,
    Str1 = string:copies("x", Len1),
    ?assertError(badarg, erlcfg_util:strftime(Str1, 1500000001, local)).

strftime_bin_test() ->
    ?assertEqual(<<"~/file.${USER}.log.20170714-024001">>,
                 erlcfg_util:strftime(<<"~/file.${USER}.log.%Y%m%d-%H%M%S">>, 1500000001, utc)),
    ?assertEqual(<<"~/file.${USER}.log.20170713-224001">>,
                 erlcfg_util:strftime(<<"~/file.${USER}.log.%Y%m%d-%H%M%S">>, 1500000001, local)).

pathftime_str_test() ->
    Home = erlcfg_util:strenv("$HOME"),
    User = os:getenv("USER"),
    Exp1 = Home ++ "/file." ++ User ++ ".log.20170714-024001",
    Exp2 = Home ++ "/file." ++ User ++ ".log.20170713-224001",
    ?assertEqual(Exp1, erlcfg_util:pathftime("~/file.${USER}.log.%Y%m%d-%H%M%S", 1500000001, utc)),
    ?assertEqual(Exp2, erlcfg_util:pathftime("~/file.${USER}.log.%Y%m%d-%H%M%S", 1500000001, local)).

pathftime_bin_test() ->
    Home = erlcfg_util:strenv("$HOME"),
    OSVar= "ABC", os:set_env_var(OSVar, OSVar),
    User = os:getenv("USER"),
    Exp1 = list_to_binary(Home ++ "/file." ++ User ++ ".log.20170714-024001"),
    Exp2 = list_to_binary(Home ++ "/file." ++ User ++ ".log.20170713-224001"),
    ?assertEqual(Exp1, erlcfg_util:pathftime(<<"~/file.${USER}.log.%Y%m%d-%H%M%S">>, 1500000001, utc)),
    ?assertEqual(Exp2, erlcfg_util:pathftime(<<"~/file.${USER}.log.%Y%m%d-%H%M%S">>, 1500000001, local)).

