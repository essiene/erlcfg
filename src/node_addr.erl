-module(node_addr).
-export([
        basename/1,
        parent/1,
        emancipate/1
    ]).

-export([
        join/1,
        split/1
    ]).

basename(NodeAddr) when is_atom(NodeAddr) ->
    {_Parent, Child} = emancipate(NodeAddr),
    Child.

parent(NodeAddr) when is_atom(NodeAddr) ->
    {Parent, _Child} = emancipate(NodeAddr),
    Parent.

join([]) ->
    '';
join(['', Single]) when is_atom(Single) ->
    Single;
join([H | _Rest]=NodeAddrList) when is_list(NodeAddrList), is_atom(H) ->
    AtomList2StrList = fun (Item) ->
            atom_to_list(Item)
    end,

    StrList = lists:map(AtomList2StrList, NodeAddrList),
    StrAddr = string:join(StrList, "."),
    list_to_atom(StrAddr).


split(NodeAddr) when is_atom(NodeAddr) ->
    StrAddr = atom_to_list(NodeAddr),
    StrList = string:tokens(StrAddr, "."),

    StrList2AtomList = fun (Item) -> 
            list_to_atom(Item)
    end,

    lists:map(StrList2AtomList, StrList).


emancipate('') ->
    invalid;

emancipate(NodeAddr) when is_atom(NodeAddr) ->
    List = split(NodeAddr),
    {ParentList, [Child]} = lists:split(length(List) - 1, List),
    {join(ParentList), Child}.
