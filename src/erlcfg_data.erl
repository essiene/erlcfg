-module(erlcfg_data, [Data]).
-export([
        raw/0,
        get/1
    ]).


raw() ->
    Data.


get(Key) ->
    interp_data:get(Data, Key).
