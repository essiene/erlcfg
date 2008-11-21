-module(erlcfg_node).
-export([
        new/0,
        get/2,
        set/3
    ]).

-export([
        node_find/2,
        node_find/3,
        node_set/3,
        node_get/2
    ]).



new() ->
    [].


set(IData, Key, Value) when is_atom(Key) ->
    Fun = fun(Node, NodeKey) -> 
            node_set(Node, NodeKey, Value)
    end,
    if_parent_found(IData, Key, Fun).

get(IData, Key) when is_atom(Key) ->
    Fun = fun(Node, NodeKey) ->
            node_get(Node, NodeKey)
    end,
    if_parent_found(IData, Key, Fun).

node_set(Node, Key, Value) when is_list(Node), is_atom(Key) ->
    lists:keystore(Key, 1, Node, {Key, Value}).

node_get(Node, Key) when is_list(Node), is_atom(Key) ->
    case lists:keysearch(Key, 1, Node) of
        false ->
            {error, undefined};
        {value, {Key, Value}} ->
            {value, Value}
    end.


node_find(IData, Addr) when is_atom(Addr) ->
    AddrList = node_addr:split(Addr),

    case node_find(AddrList, [], IData) of
        {node, Node} -> 
            {node, Node};
        {not_found, ErrorAddrList} ->
            {not_found, node_addr:join(ErrorAddrList)}
    end.

node_find([], _ProcessedKeys, Node) ->
    {node, Node};

node_find([H | Rest], ProcessedKeys, IData) ->
    case lists:keysearch(H, 1, IData) of
        {value, {H, NestedNode}} ->
            node_find(Rest, [H | ProcessedKeys], NestedNode);
        false ->
            {not_found, lists:reverse(ProcessedKeys)}
    end.

if_parent_found(IData, Key, Fun) ->
    Parent = node_addr:parent(Key),
    case node_find(IData, Parent) of
        {node, Node} ->
            ChildKey = node_addr:basename(Key),
            Fun(Node, ChildKey);
        {not_found, ErrorNode} ->
            {not_found, ErrorNode}
    end.

