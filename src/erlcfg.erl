-module(erlcfg).
-export([
        get/3
    ]).



get([], ProcessedKeys, Value) ->
    {value, {lists:reverse(ProcessedKeys), Value}};

get([H | Rest], ProcessedKeys, TupleList) ->
    case lists:keysearch(H, 1, TupleList) of
        {value, {H, NestedValue}} ->
            get(Rest, [H | ProcessedKeys], NestedValue);
        false ->
            {error, {not_found, lists:reverse(ProcessedKeys)}}
    end.
