-module(erlcfg_node).
-export([
        new/0,
        get/2,
        set/3
    ]).

-export([
        node_find/2,
        node_find/3,
        node_add/2,
        node_add/3,
        node_read/2,
        if_parent_found/3,
        if_parent_found/5,
        walk_tree_set_node/5
    ]).



new() ->
    {c, '', []}.


set(IData, Key, Value) when is_atom(Key) ->
    if_parent_found(IData, Key, ?MODULE, walk_tree_set_node, [IData, Key, Value]).

walk_tree_set_node(Parent, ChildName, Parent, _Key, Value) ->
    node_add(Parent, ChildName, Value);

walk_tree_set_node(Parent, ChildName, IData, Key, Value) -> 
    NewValue = node_add(Parent, ChildName, Value), 
    NewKey = node_addr:parent(Key),
    if_parent_found(IData, NewKey, ?MODULE, walk_tree_set_node, [IData, NewKey, NewValue]).


get(IData, Key) when is_atom(Key) ->
    Fun = fun(Parent, ChildName) ->
            node_read(Parent, ChildName)
    end,
    if_parent_found(IData, Key, Fun).


node_add({_Type, _ParentName, _Container}=ParentNode, Key, Value) ->
    node_add(ParentNode, Key, Value, d).

node_add({_Type, _ParentName, _Container}=ParentNode, Key) ->
    node_add(ParentNode, Key, [], c).

node_add({c, ParentName, Container}, Key, Value, Type) when is_list(Container), is_atom(Key) ->
    NewContainer = lists:keystore(Key, 2, Container, {Type, Key, Value}),
    {c, ParentName, NewContainer};

node_add({d, _ParentName, _Container}, _Key, _Value, _Type) ->
    {error, data_node_add}.

node_read({c, _ParentName, Container}, Key) when is_list(Container), is_atom(Key) ->
    case lists:keysearch(Key, 2, Container) of
        false ->
            {error, undefined};
        {value, {_Type, Key, Value}} ->
            {value, Value}
    end;

node_read({d, _ParentName, _Container}, _Key) ->
    {error, data_node_read}.


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

if_parent_found(IData, Key, Mod, Fun, Args) ->
    Fun_Mfa = fun(Parent, ChildName) -> 
            apply(Mod, Fun, [Parent, ChildName | Args])
    end,
    if_parent_found(IData, Key, Fun_Mfa).
