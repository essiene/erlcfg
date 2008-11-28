-module(erlcfg_data, [Node]).
-export([
        raw/0,
        get/1,
        get/2
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
