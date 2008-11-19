-module(interp_data).
-export([
        get/2,
        get/3
    ]).


get(NestedTupleList, Key) when is_atom(Key) ->
    StrKey = atom_to_list(Key),
    StrKeyList = string:tokens(StrKey, "."),
    KeyList = lists:map(
        fun(Item) -> list_to_atom(Item) end,
        StrKeyList
    ),
    
    case get(KeyList, [], NestedTupleList) of
        {value, {KeyList, Value}} -> 
            {value, Value};
        {error, {Reason, ErrorKeyList}} ->
            StrErrorKeyList = lists:map(
                fun(Item) -> atom_to_list(Item) end,
                ErrorKeyList
            ),
            ErrorStrKey = string:join(StrErrorKeyList, "."),
            ErrorKey = list_to_atom(ErrorStrKey),
            {error, {Reason, ErrorKey}}
    end.

get([], ProcessedKeys, Value) ->
    {value, {lists:reverse(ProcessedKeys), Value}};

get([H | Rest], ProcessedKeys, TupleList) ->
    case lists:keysearch(H, 1, TupleList) of
        {value, {H, NestedValue}} ->
            get(Rest, [H | ProcessedKeys], NestedValue);
        false ->
            {error, {not_found, lists:reverse(ProcessedKeys)}}
    end.
