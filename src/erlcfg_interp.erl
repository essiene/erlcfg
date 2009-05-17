-module(erlcfg_interp).
-export([eval/1, eval/2]).
-include("erlcfg.hrl").


eval(Ast) ->
    InterpState = #interp{node=erlcfg_node:new()},
    eval(Ast, InterpState).

eval(#get{address=Address}, #interp{node=Node}=State) ->
    case erlcfg_node:get(Node, Address) of
        {value, Value} ->
            State#interp{value=Value};
        {not_found, InvalidAddress} ->
            throw({not_found, InvalidAddress})
    end;

eval(#set{key=Key, value=Value, next=Next}, #interp{scope=Scope}=State) ->
    StateAfterValueEval = eval(Value, State),

    ScopedKey = node_addr:join([Scope, Key]),

    case erlcfg_node:set(StateAfterValueEval#interp.node, ScopedKey, StateAfterValueEval#interp.value) of
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
            EvalState = eval(Child, State#interp{node=NewNode, scope=ScopedName}),
            eval(Next, EvalState#interp{scope=Scope})
    end;

eval(nil, State) ->
    State;

% an empty list
eval([], State) ->
    State#interp{value=[]};

eval(Data, State) when is_record(Data, cons) ->
    State#interp{value=cons(Data, State)};

eval(Data, State) when is_binary(Data) ->
    State#interp{value=binary_to_list(Data)};

eval(Data, State) when is_number(Data); is_atom(Data) ->
    State#interp{value=Data};

eval(Unknown, _State) -> % TODO: capture current scope?
    throw({illegal_command, Unknown}).

cons(#cons{head=Head, tail=[]}, State) ->
    NewState = eval(Head, State),
    [NewState#interp.value];
cons(#cons{head=Head, tail=Tail}, State) ->
    NewState = eval(Head, State),
    [NewState#interp.value | cons(Tail, NewState)].
