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
        prepare/1
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
    
