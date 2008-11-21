-module(erlcfg_data, [Node]).
-export([
        raw/0,
        get/1
    ]).


raw() ->
    Node.


get(Key) ->
    erlcfg_node:get(Node, Key).
