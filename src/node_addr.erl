-module(node_addr).
-export([
        split/1,
        join/1
    ]).


split(NodeAddr) when is_atom(NodeAddr) ->
    StrAddr = atom_to_list(NodeAddr),
    StrList = string:tokens(StrAddr, "."),

    StrList2AtomList = fun (Item) -> 
            list_to_atom(Item)
    end,

    lists:map(StrList2AtomList, StrList).

join([H | _Rest]=NodeAddrList) when is_list(NodeAddrList), is_atom(H) ->
    AtomList2StrList = fun (Item) ->
            atom_to_list(Item)
    end,

    StrList = lists:map(AtomList2StrList, NodeAddrList),
    StrAddr = string:join(StrList, "."),
    list_to_atom(StrAddr).


