-module(erlcfg_data, [Node]).
-export([
        raw/0,
        get/1
    ]).


raw() ->
    Node.


get(Key) ->
    convert(erlcfg_node:get(Node, Key)).

convert({value, Value}) when is_binary(Value) ->
    binary_to_list(Value);

convert({value, Value}) ->
    Value.
