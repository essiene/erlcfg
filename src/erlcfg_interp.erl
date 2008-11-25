-module(erlcfg_interp).
-export([
        new/0,
        eval/2
    ]).


new() ->
    erlcfg_node:new().

eval(State, {val, Data, _Ignore}) ->
    {State, Data};

eval(State, {get, Address, _Ignore}) ->
   case erlcfg_node:get(State, Address) of
       {State, Value} ->
           {State, Value};
       {not_found, InvalidAddress} ->
           throw({not_found, InvalidAddress})
   end;

eval(State, {set, Address, Value}) ->
    case erlcfg_node:set(State, Address, Value) of
        {not_found, InvalidAddress} ->
            throw({not_found, InvalidAddress});
        NewState ->
            {NewState, Value}
    end;

eval(State, Data) when is_number(Data); is_atom(Data); is_binary(Data) ->
    {State, Data};

eval(_State, Unknown) ->
    throw({illegal_command, Unknown}).
