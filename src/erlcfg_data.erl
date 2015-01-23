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
        get/1,
        get/2,
        raw_get/1,
        raw_get/2,
        get_config/1,
        ensure_get/1,
        prepare/1,
        keys/0,
        keys/1,
        data/0,
        data/1,
        data/2,
        children/0,
        children/1
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
        

raw_get(Key) ->
    case erlcfg_node:get(Node, Key) of
        {not_found, MissingNode} ->
            {error, {not_found, MissingNode}};
        {value, Value} ->
            THIS:prepare(Value)
    end.

raw_get(Key, Default) ->
    case THIS:raw_get(Key) of
        {error, _Reason} ->
            Default;
        Value ->
            Value
    end.

get(Key, Default) ->
    Val = THIS:raw_get(Key, Default),
    find_and_convert_string(Val).

get(Key) ->
    Val = THIS:raw_get(Key),
    find_and_convert_string(Val).

ensure_get(Key) ->
    case THIS:get(Key) of
        {error, Reason} ->
            throw(Reason);
        Value ->
            THIS:prepare(Value)
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
prepare(Value) ->
    Value.


get_config(Key) ->
    {erlcfg_data, {c, '', THIS:ensure_get(Key)}}.

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
    
