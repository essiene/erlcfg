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

eval(Value, State) ->
    {State, Value}.
