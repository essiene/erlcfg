-module(erlcfg_data, [Node]).
-export([
        raw/0,
        get/1,
        get/2,
        ensure_get/1
    ]).


raw() ->
    Node.

get(Key, Default) ->
    case THIS:get(Key) of
        {error, _Reason} ->
            Default;
        Value ->
            Value
    end.

get(Key) ->
    case erlcfg_node:get(Node, Key) of
        {not_found, MissingNode} ->
            {error, {not_found, MissingNode}};
        {value, Value} ->
            Value
    end.

ensure_get(Key) ->
    case THIS:get(Key) of
        {error, Reason} ->
            throw(Reason);
        Value ->
            Value
    end.
