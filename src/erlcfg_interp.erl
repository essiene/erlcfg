-module(erlcfg_interp).
-export([
        new/0,
        eval/3
    ]).


new() ->
    erlcfg_node:new().

eval(State, CurrentBlock, {get, Address}) ->
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

    FullAddress = node_addr:join([CurrentBlock, Address]),
    case erlcfg_node:set(State, FullAddress) of
        {not_found, InvalidAddress} ->
            throw({not_found, InvalidAddress});
        NewState2 ->
            {NewState2, FullAddress, []}
    end;

eval(State, CurrentBlock, {endblock, _Ignore, _Ignore}) ->
    NewBlock = node_addr:parent(CurrentBlock),
    {State, NewBlock, CurrentBlock};

eval(State, CurrentBlock, {cons, Elem, List}) ->
    {NewState, CurrentBlock, ElemValue} = eval(State, CurrentBlock, Elem),
    {NewState1, CurrentBlock, ListValue} = eval(NewState, CurrentBlock, List),
    {NewState1, CurrentBlock, [ElemValue | ListValue]};

eval(State, CurrentBlock, nil) ->
    {State, CurrentBlock, []};

eval(State, CurrentBlock, []) ->
    {State, CurrentBlock, []};

eval(State, CurrentBlock, Data) when is_binary(Data) ->
    {State, CurrentBlock, binary_to_list(Data)};

eval(State, CurrentBlock, Data) when is_number(Data); is_atom(Data) ->
    {State, CurrentBlock, Data};

eval(_State, _CurrentBlock, Unknown) -> % TODO: reflect current block in error msg?
    throw({illegal_command, Unknown}).
