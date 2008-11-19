-module(erlcfg_data, [Data]).
-export([
        raw/0,
        get/1
    ]).


raw() ->
    Data.

get(Key) when is_atom(Key) ->
    StrKey = atom_to_list(Key),
    KeyList = string:tokens(StrKey, "."),
    case erlcfg:get(KeyList, [], Data) of
        {value, {KeyList, Value}} -> 
            {value, Value};
        {error, {Reason, ErrorKeyList}} ->
            ErrorStrKey = string:join(ErrorKeyList, "."),
            ErrorKey = list_to_atom(ErrorStrKey),
            {error, {Reason, ErrorKey}}
    end.
