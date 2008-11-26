-module(erlcfg_interp).
-export([
        new/0,
        eval/3
    ]).


new() ->
    erlcfg_node:new().

eval(State, CurrentBlock, {val, Data, _Ignore}) ->
    eval(State, CurrentBlock, Data);

eval(State, CurrentBlock, {get, Address, _Ignore}) ->
   case erlcfg_node:get(State, Address) of
       {value, Value} ->
           {State, CurrentBlock, Value};
       {not_found, InvalidAddress} ->
           throw({not_found, InvalidAddress})
   end;

eval(State, CurrentBlock, {set, Address, Value}) ->
    {NewState, NewBlock, Op2Value} = eval(State, CurrentBlock, Value),

    FullAddress = node_addr:join([NewBlock, Address]),
    case erlcfg_node:set(NewState, FullAddress, Op2Value) of
        {not_found, InvalidAddress} ->
            throw({not_found, InvalidAddress});
        NewState2 ->
            {NewState2, NewBlock, Op2Value}
    end;

eval(State, CurrentBlock, Data) when is_number(Data); is_atom(Data); is_binary(Data) ->
    {State, CurrentBlock, Data};

eval(_State, _CurrentBlock, Unknown) -> % TODO: reflect current block in error msg?
    throw({illegal_command, Unknown}).
