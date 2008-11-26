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
    {NewState, CurrentBlock, Op2Value} = eval(State, CurrentBlock, Value),

    FullAddress = node_addr:join([CurrentBlock, Address]),
    case erlcfg_node:set(NewState, FullAddress, Op2Value) of
        {not_found, InvalidAddress} ->
            throw({not_found, InvalidAddress});
        NewState2 ->
            {NewState2, CurrentBlock, Op2Value}
    end;

eval(State, CurrentBlock, {block, Address, _Ignore}) ->
    FullAddress = node_addr:join([CurrentBlock, Address]),
    {NewState, CurrentBlock, []} = eval(State, CurrentBlock, {set, FullAddress, []}),
    {NewState, FullAddress, []};

eval(State, CurrentBlock, {endblock, _Ignore, _Ignore}) ->
    NewBlock = node_addr:parent(CurrentBlock),
    {State, NewBlock, CurrentBlock};

eval(State, CurrentBlock, []) ->
    {State, CurrentBlock, []};

eval(State, CurrentBlock, Data) when is_number(Data); is_atom(Data); is_binary(Data) ->
    {State, CurrentBlock, Data};

eval(_State, _CurrentBlock, Unknown) -> % TODO: reflect current block in error msg?
    throw({illegal_command, Unknown}).
