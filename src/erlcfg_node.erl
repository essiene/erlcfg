-module(erlcfg_node).
-export([
        new/0,
        get/2,
        set/3
    ]).

-export([
        node_find/2,
        node_find/3,
        node_write/2,
        node_write/3,
        node_read/1,
        node_read/2,
        if_node_found/3,
        if_node_found/5,
        walk_tree_set_node/5
    ]).



new() ->
    {c, '', []}.


set(IData, Address, Value) when is_atom(Address) ->
    if_node_found(IData, Address, ?MODULE, walk_tree_set_node, [IData, Address, Value]).

walk_tree_set_node(Parent, ChildName, Parent, _Key, Value) ->
    node_write(Parent, ChildName, Value);

walk_tree_set_node(Parent, ChildName, IData, Key, Value) -> 
    NewValue = node_write(Parent, ChildName, Value), 
    NewKey = node_addr:parent(Key),
    if_node_found(IData, NewKey, ?MODULE, walk_tree_set_node, [IData, NewKey, NewValue]).


get(IData, Address) when is_atom(Address) ->
    Fun = fun(Node) ->
            node_read(Node)
    end,
    if_node_found(IData, Address, Fun).


node_write({c, _ParentName, _Container}=ParentNode, Key, Value) ->
    node_write(ParentNode, Key, Value, d);

node_write({d, _ParentName, _Container}, _Key, _Value) ->
    {error, data_node_write_child}.

node_write({c, _ParentName, _Container}=ParentNode, Key) ->
    node_write(ParentNode, Key, [], c);

node_write({d, NodeName, _OldValue}, Value) ->
    {d, NodeName, Value}.

node_write({c, ParentName, Container}, Key, Value, Type) when is_list(Container), is_atom(Key) ->
    NewContainer = lists:keystore(Key, 2, Container, {Type, Key, Value}),
    {c, ParentName, NewContainer}.


node_read({c, _ParentName, Container}, Key) when is_list(Container), is_atom(Key) ->
    case lists:keysearch(Key, 2, Container) of
        false ->
            {error, undefined};
        {value, {_Type, Key, Value}} ->
            {value, Value}
    end;

node_read({d, _ParentName, _Container}, _Key) ->
    {error, data_node_read_child}.

node_read({_Type, _NodeName, Value}) ->
    {value, Value}.



node_find(IData, Addr) when is_atom(Addr) ->
    AddrList = node_addr:split(Addr),

    case node_find(AddrList, [], IData) of
        {not_found, ErrorAddrList} ->
            {not_found, node_addr:join(ErrorAddrList)};
        Node -> 
            Node
    end.

node_find([]=_RemainingKeys, _ProcessedKeys, Node) ->
    Node;

node_find([CurrentKey | Rest]=_RemainingKeys, ProcessedKeys, {c, _ParentName, Container}) when is_list(Container) ->
    case lists:keysearch(CurrentKey, 2, Container) of
        {value, {_Type, CurrentKey, _Value}=CurrentNode} ->
            node_find(Rest, [CurrentKey | ProcessedKeys], CurrentNode);
        false ->
            {not_found, lists:reverse([CurrentKey | ProcessedKeys])}
    end;

node_find([CurrentKey | _Rest]=_RemainingKeys, ProcessedKeys, {d, _ParentName, _Value}) ->
    {not_found, lists:reverse([CurrentKey | ProcessedKeys])}.


if_node_found(IData, Address, Fun) ->
    case node_find(IData, Address) of
        {not_found, InvalidAddress} ->
            {not_found, InvalidAddress};
        Node ->
            Fun(Node)
    end.

if_node_found(IData, Address, Mod, Fun, Args) ->
    Fun_Mfa = fun(Node) -> 
            apply(Mod, Fun, [Node | Args])
    end,
    if_node_found(IData, Address, Fun_Mfa).
