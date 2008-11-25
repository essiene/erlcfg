-module(erlcfg_interp).
-export([
        new/0,
        eval/2
    ]).


new() ->
    erlcfg_node:new().

eval(State, {val, Data, _Ignore}) ->
    eval(State, Data);

eval(State, {get, Address, _Ignore}) ->
   case erlcfg_node:get(State, Address) of
       {value, Value} ->
           {State, Value};
       {not_found, InvalidAddress} ->
           throw({not_found, InvalidAddress})
   end;

eval(State, {set, Address, Value}) ->
    {NewState, Op2Value} = eval(State, Value),

    case erlcfg_node:set(NewState, Address, Op2Value) of
        {not_found, InvalidAddress} ->
            throw({not_found, InvalidAddress});
        NewState2 ->
            {NewState2, Op2Value}
    end;

eval(State, Data) when is_number(Data); is_atom(Data); is_binary(Data) ->
    {State, Data};

eval(_State, Unknown) ->
    throw({illegal_command, Unknown}).
