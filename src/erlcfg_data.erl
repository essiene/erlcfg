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

-include_lib("pmod_transform/include/pmod.hrl").
-module(erlcfg_data, [Node]).
-export([
        raw/0,
        create/2,
        set/2,
        set_default/2,
        get/1,
        get/2,
        exists/1,
        raw_get/1,
        raw_get/2,
        split/1,
        get_config/1,
        ensure_get/1,
        prepare/1,
        keys/0,
        keys/1,
        data/0,
        data/1,
        data/2,
        children/0,
        children/1,
        print/0,
        print/1,
        print/2,
        walk/1,
        set_app_env/0,
        set_app_env/1,
        set_app_env/2,
        set_app_env/3
    ]).


raw() ->
    Node.

create(Key, Value) ->
    create_node(Node, Key, Value).

create_node(ANode, Key, Value) ->
    case erlcfg_node:set(ANode, Key, Value) of
        {not_found, MissingNode} ->
            ANewNode = erlcfg_node:set(ANode, MissingNode),
            create_node(ANewNode, Key, Value);
        NewNode ->
            erlcfg_data:new(NewNode)
    end.
            
set(Key, Value) ->
    case erlcfg_node:set(Node, Key, Value) of
        {not_found, MissingNode} ->
            {error, {not_found, MissingNode}};
        {error, Reason} ->
            {error, Reason};
        NewNode ->
            erlcfg_data:new(NewNode)
    end.

%% @doc Set `Value' of `Key' if the `Key' is not found in config
set_default(Key, Value) ->
    case erlcfg_node:if_node_found(Node, Key, fun(_) -> Node end) of
        {not_found, _MissingNode} ->
            set(Key, Value);
        {error, Reason} ->
            {error, Reason};
        _Tree ->
            THIS
    end.

raw_get(Key) ->
    case erlcfg_node:get(Node, Key) of
        {not_found, MissingNode} ->
            {error, {not_found, MissingNode}};
        {value, Value} ->
            prepare(Value)
    end.

raw_get(Key, Default) ->
    case raw_get(Key) of
        {error, _Reason} ->
            Default;
        Value ->
            Value
    end.

exists(Key) ->
    case raw_get(Key) of
        {error, _} -> false;
        _          -> true
    end.

get(Key, Default) ->
    Val = raw_get(Key, Default),
    find_and_convert_string(Val).

get(Key) ->
    Val = raw_get(Key),
    find_and_convert_string(Val).

ensure_get(Key) ->
    case get(Key) of
        {error, Reason} ->
            throw(Reason);
        Value ->
            prepare(Value)
    end.

%% @doc Split a subtree from the configuration tree.
%% This function preserves the tree under the basename of `Key'.
split(Key) ->
    case get(Key) of
        {error, Reason} ->
            throw(Reason);
        {erlcfg_data, Value} ->
            K = erlcfg_node_addr:basename(Key),
            V = erlcfg_node:new(Value, K),
            prepare(V)
    end.

keys() ->
    {c, '', List} = Node,
    [K || {_,K,_} <- List].

keys(Key) ->
    {erlcfg_data, {c, '', List}} = raw_get(Key),
    [K || {_,K,_} <- List].

data() ->
    {c, '', List} = Node,
    [{K,V} || {d,K,V} <- List].

data(Key) ->
    {erlcfg_data, {c, '', List}} = raw_get(Key),
    [{K,V} || {d,K,V} <- List].

data(Key, Default) when is_list(Default) ->
    try
        {erlcfg_data, {c, '', List}} = raw_get(Key),
        [{K,V} || {d,K,V} <- List]
    catch _:_ ->
        Default
    end.

children() ->
    {c, '', List} = Node,
    [K || {c,K,_} <- List].

children(Key) ->
    {erlcfg_data, {c, '', List}} = raw_get(Key),
    [K || {c,K,_} <- List].

prepare([{c, _K, _V} | _Rest]=Value) ->
    {erlcfg_data, {c, '', Value}};
prepare([{d, _K, _V} | _Rest]=Value) ->
    {erlcfg_data, {c, '', Value}};
prepare({c, '', _Value} = T) ->
    {erlcfg_data, T};
prepare(Value) ->
    Value.


get_config(Key) ->
    {erlcfg_data, {c, '', ensure_get(Key)}}.

find_and_convert_string(Value) when is_list(Value) ->
    find_and_convert_string(Value, []);
find_and_convert_string(Value) when is_binary(Value) -> 
    binary_to_list(Value);
find_and_convert_string(Value) ->
    Value.

find_and_convert_string([], Acc) ->
    lists:reverse(Acc);
find_and_convert_string([Head|Rest], Acc) ->
    find_and_convert_string(Rest, [find_and_convert_string(Head) | Acc]).
    
print() ->
    print("").
print(Prefix) ->
    KeyFun = fun(node, L)     -> string:join(lists:reverse([atom_to_list(I) || I <- L]), "/");
                (value,[K|_]) -> atom_to_list(K)
             end,
    print(Prefix, KeyFun).
print(Prefix, KeyFun) ->
    Fun = fun(node,     Indent,RevPath) ->
                io:format("~s~*c/~s\n",     [Prefix, Indent*2, $\s, KeyFun(node, RevPath)]);
             ({value,V},Indent,RevPath) ->
                io:format("~s~*c~s = ~p\n", [Prefix, Indent*2, $\s, KeyFun(value,RevPath),V])
          end,
    walk(Fun).

walk(Fun) ->
    walk(Node, 0, [], Fun).

walk({c,_,[]},_,_,_)   -> ok;
walk({c,_N,L},Indent,RevPath,Fun) ->
    Fun(node, Indent,RevPath),
    [walk(II,Indent+1,[element(2,II)|RevPath],Fun) || II <- L],
    ok;
walk({d,_K,V},Indent,RevPath,Fun) ->
    Fun({value, V}, Indent, RevPath).

set_app_env()    -> set_app_env(application:get_application()).
set_app_env(App) -> set_app_env(App, []).
set_app_env(App, RemoveKeyPrefix) -> set_app_env(App, RemoveKeyPrefix, undefined).
set_app_env(App, RemoveKeyPrefix, Filter) when [] == RemoveKeyPrefix
                                             ; is_list(RemoveKeyPrefix)
                                             , is_atom(hd(RemoveKeyPrefix)) ->
    Fun = fun(node,     _Indent,_RevPath) -> ok;
             ({value,V},_Indent, RevPath) ->
              Path = remove_key_prefix(lists:reverse(RevPath), RemoveKeyPrefix),
              case Filter==undefined orelse Filter(RevPath) of
                  true ->
                      application:set_env(
                          App,
                          list_to_atom(string:join([atom_to_list(I) || I <- Path], ".")),
                          V);
                  false ->
                      ok
              end
          end,
    walk(Fun).

remove_key_prefix([], _) -> [];
remove_key_prefix(L, []) -> L;
remove_key_prefix([H|T1],[H|T2]) -> remove_key_prefix(T1, T2);
remove_key_prefix(Key,_) -> Key.

