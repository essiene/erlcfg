-module(erlcfg_interp).
-export([interpret/1]).
-export([eval/3, rhs/2]).
-include("erlcfg.hrl").


interpret(AstList) ->
    State = #interp{node=erlcfg_node:new()},
    Scope = '',
    {ok, interpret(AstList, Scope, State)}.

interpret([], _Scope, State) ->
    State;
interpret([Head|Rest], Scope, State) ->
    interpret(Head, Rest, Scope, State).

interpret(Current, [], Scope, State) ->
    eval(Current, Scope, State);
interpret(Current, [Head|Rest], Scope, State0) ->
    State1 = eval(Current, Scope, State0),
    interpret(Head, Rest, Scope, State1).


eval(#set{key=Key, value=Value}, Scope, #interp{}=State0) ->
    State1 = rhs(Value, State0),

    ScopedKey = node_addr:join([Scope, Key]),

    case erlcfg_node:set(State1#interp.node, ScopedKey, State1#interp.value) of
        {not_found, InvalidAddress} ->
            throw({not_found, InvalidAddress});
        NewNode ->
            State1#interp{node=NewNode}
    end;

eval(#block{name=Name, children=Children}, Scope, #interp{node=Node}=State0) ->
    ScopedName = node_addr:join([Scope, Name]),

    case erlcfg_node:set(Node, ScopedName) of
        {not_found, InvalidAddress} ->
            throw({not_found, InvalidAddress});
        NewNode ->
            State1 = State0#interp{node=NewNode},
            interpret(Children, ScopedName, State1)
    end.



rhs(#get{address=Address}, #interp{node=Node}=State) ->
    case erlcfg_node:get(Node, Address) of
        {value, Value} ->
            State#interp{value=Value};
        {not_found, InvalidAddress} ->
            throw({not_found, InvalidAddress})
    end;

rhs(#list{data=nil}, State) ->
    State#interp{value=[]};

rhs(#list{data=Data}, State) ->
    State#interp{value=cons(Data, State)};

rhs(Data, State) when is_number(Data); is_atom(Data); is_boolean(Data); is_binary(Data) ->
    State#interp{value=Data};

rhs(Unknown, _State) -> % TODO: capture current scope?
    throw({unsupported_value_type, Unknown}).


cons(#cons{head=Head, tail=nil}, State) ->
    NewState = rhs(Head, State),
    [NewState#interp.value];
cons(#cons{head=Head, tail=Tail}, State) ->
    NewState = rhs(Head, State),
    [NewState#interp.value | cons(Tail, NewState)].
