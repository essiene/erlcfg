-module(erlcfg_interp2).
-export([eval/1, eval/2]).
-include("erlcfg.hrl").


eval(Ast) ->
    InterpState = #interp{node=erlcfg_node:new(), scope=''},
    {EvalState, _Value} = eval(Ast, InterpState),
    EvalState#interp.node.

eval(#get{address=Address}, #interp{node=Node}=State) ->
    case erlcfg_node:get(Node, Address) of
        {value, Value} ->
            {State, Value};
        {not_found, InvalidAddress} ->
            throw({not_found, InvalidAddress})
    end;

eval(#set{key=Key, value=Value, next=Next}, #interp{scope=Scope}=State) ->
    {StateAfterValueEval, EvalValue} = eval(Value, State),

    ScopedKey = node_addr:join([Scope, Key]),

    case erlcfg_node:set(StateAfterValueEval#interp.node, ScopedKey, EvalValue) of
        {not_found, InvalidAddress} ->
            throw({not_found, InvalidAddress});
        NewNode ->
            eval(Next, StateAfterValueEval#interp{node=NewNode})
    end;

eval(#block{name=Name, child=Child, next=Next}, #interp{node=Node, scope=Scope}=State) ->
    ScopedName = node_addr:join([Scope, Name]),

    case erlcfg_node:set(Node, ScopedName) of
        {not_found, InvalidAddress} ->
            throw({not_found, InvalidAddress});
        NewNode ->
            {EvalState, _Value} = eval(Child, State#interp{node=NewNode, scope=ScopedName}),
            eval(Next, EvalState#interp{scope=Scope})
    end;

eval(nil, State) ->
    {State, []};

eval([], State) ->
    {State, []};

eval(Data, State) when is_record(Data, cons) ->
    {State, cons(Data)};

eval(Data, State) when is_binary(Data) ->
    {State, binary_to_list(Data)};

eval(Data, State) when is_number(Data); is_atom(Data) ->
    {State, Data};

eval(Unknown, _State) -> % TODO: capture current scope?
    throw({illegal_command, Unknown}).

cons(#cons{head=Head, tail=nil}) ->
    [Head];
cons(#cons{head=Head, tail=Tail}) ->
    [Head | cons(Tail)].
