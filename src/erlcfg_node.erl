%% 
%% Copyright (c) 2008-2010, Essien Ita Essien
%% All rights reserved.
%% 
%% Redistribution and use in source and binary forms, 
%% with or without modification, are permitted 
%% provided that the following conditions are met:
%%
%%    * Redistributions of source code must retain the 
%%      above copyright notice, this list of conditions 
%%      and the following disclaimer.
%%    * Redistributions in binary form must reproduce 
%%      the above copyright notice, this list of 
%%      conditions and the following disclaimer in the 
%%      documentation and/or other materials provided with 
%%      the distribution.
%%    * Neither the name "JsonEvents" nor the names of its 
%%      contributors may be used to endorse or promote 
%%      products derived from this software without 
%%      specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND 
%% CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED 
%% WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
%% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR 
%% PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT 
%% HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, 
%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES 
%% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE 
%% GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR 
%% BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF 
%% LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT 
%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF 
%% THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY 
%% OF SUCH DAMAGE.
%% 

-module(erlcfg_node).
-export([
        new/0,
        new/2,
        get/2,
        set/2,
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
        walk_tree_set_node/4,
        walk_tree_set_node/5
    ]).



new() ->
    {c, '', []}.

new({c, '', Data}, Key) ->
    {c, '', [{c, Key, Data}]}.

set(IData, Address, Value) when is_atom(Address); is_list(Address) ->
    {ParentAddress, Key} = erlcfg_node_addr:emancipate(Address),
    if_node_found(IData, ParentAddress, ?MODULE, walk_tree_set_node, [IData, ParentAddress, Key, Value]).

set(IData, Address) when is_atom(Address); is_list(Address) ->
    {ParentAddress, Key} = erlcfg_node_addr:emancipate(Address),
    if_node_found(IData, ParentAddress, ?MODULE, walk_tree_set_node, [IData, ParentAddress, Key]).

walk_tree_set_node(Node, _IData, '', Key, Value) ->
    node_write(Node, Key, Value);


walk_tree_set_node(Node, IData, Address, Key, Value) -> 
    {c, _NodeName, _Container=NewValue} = node_write(Node, Key, Value), 
    {ParentAddress, NewKey} = erlcfg_node_addr:emancipate(Address),
    if_node_found(IData, ParentAddress, ?MODULE, walk_tree_set_node, [IData, ParentAddress, NewKey, NewValue]).

walk_tree_set_node(Node, _IData, '', Key) ->
    node_write(Node, Key);

walk_tree_set_node(Node, IData, Address, Key) -> 
    {c, _NodeName, _Container=NewValue} = node_write(Node, Key), 
    {ParentAddress, NewKey} = erlcfg_node_addr:emancipate(Address),
    if_node_found(IData, ParentAddress, ?MODULE, walk_tree_set_node, [IData, ParentAddress, NewKey, NewValue]).

get(IData, Address) when is_atom(Address); is_list(Address) ->
    Fun = fun(Node) -> node_read(Node) end,
    if_node_found(IData, Address, Fun).

node_write({c, _ParentName, _Container}=ParentNode, Key, [Node | _Rest]=Value) when is_tuple(Node) ->
    node_write(ParentNode, Key, Value, c);

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



node_find(IData, Addr) when is_atom(Addr); is_list(Addr) ->
    AddrList = erlcfg_node_addr:split(Addr),

    case node_find(AddrList, [], IData) of
        {not_found, ErrorAddrList} ->
            {not_found, erlcfg_node_addr:join(ErrorAddrList)};
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
    {not_found, lists:reverse([CurrentKey | ProcessedKeys])};

node_find([_CurrentKey | _Rest]=_RemainingKeys, _ProcessedKeys, {not_found, InvalidAddress}) ->
    {not_found, InvalidAddress}.


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
