-module(interp_data).
-export([
        new/0,
        find/2,
        find/3,
        get/2
    ]).


new() ->
    [].


get(IData, Key) when is_atom(Key) ->
    case find(IData, Key) of
        {node, Value} ->
            {value, Value};
        {not_found, ErrorNode} ->
            {error, {not_found, ErrorNode}}
    end.

find(IData, Addr) when is_atom(Addr) ->
    AddrList = node_addr:split(Addr),

    case find(AddrList, [], IData) of
        {node, Node} -> 
            {node, Node};
        {not_found, ErrorAddrList} ->
            {not_found, node_addr:join(ErrorAddrList)}
    end.

find([], _ProcessedKeys, Node) ->
    {node, Node};

find([H | Rest], ProcessedKeys, IData) ->
    case lists:keysearch(H, 1, IData) of
        {value, {H, NestedNode}} ->
            find(Rest, [H | ProcessedKeys], NestedNode);
        false ->
            {not_found, lists:reverse(ProcessedKeys)}
    end.
