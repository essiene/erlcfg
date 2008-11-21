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
        node_get/2,
        if_parent_found/3
    ]).



new() ->
    [].


set(IData, Key, Value) when is_atom(Key) ->
    Fun = fun(Parent, ChildName) -> 
            node_set(Parent, ChildName, Value)
    end,
    if_parent_found(IData, Key, Fun).

get(IData, Key) when is_atom(Key) ->
    Fun = fun(Parent, ChildName) ->
            node_get(Parent, ChildName)
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

node_find([]=_RemainingKeys, _ProcessedKeys, IData) ->
    {node, IData};


node_find([H | Rest]=_RemainingKeys, ProcessedKeys, IData) when is_list(IData) ->
    case lists:keysearch(H, 1, IData) of
        {value, {H, NestedNode}} ->
            node_find(Rest, [H | ProcessedKeys], NestedNode);
        false ->
            {not_found, lists:reverse([H | ProcessedKeys])}
    end;

node_find([H | _Rest]=_RemainingKeys, ProcessedKeys, _IData) ->
    {not_found, lists:reverse([H | ProcessedKeys])}.


if_parent_found(IData, Key, Fun) ->
    ParentName = node_addr:parent(Key),
    case node_find(IData, ParentName) of
        {node, Parent} ->
            ChildName = node_addr:basename(Key),
            Fun(Parent, ChildName);
        {not_found, InvalidAddress} ->
            {not_found, InvalidAddress}
    end.

