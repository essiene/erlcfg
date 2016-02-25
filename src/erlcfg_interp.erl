%% 
%% Copyright (c) 2008-2010, Essien Ita Essien
%% All rights reserved.
%% 
%% Redistribution and use in source and binary forms, 
%% with or without modification, are permitted 
%% provided that the following conditions are met:
%%
%%    * Redistributions of source code must retain the 
%%      above copyright notice, this list of conditions 
%%      and the following disclaimer.
%%    * Redistributions in binary form must reproduce 
%%      the above copyright notice, this list of 
%%      conditions and the following disclaimer in the 
%%      documentation and/or other materials provided with 
%%      the distribution.
%%    * Neither the name "JsonEvents" nor the names of its 
%%      contributors may be used to endorse or promote 
%%      products derived from this software without 
%%      specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND 
%% CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED 
%% WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
%% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR 
%% PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT 
%% HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, 
%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES 
%% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE 
%% GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR 
%% BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF 
%% LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT 
%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF 
%% THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY 
%% OF SUCH DAMAGE.
%% 

-module(erlcfg_interp).
-export([interpret/1, interpret/2]).
-export([eval/3, rhs/2]).
-include("erlcfg.hrl").


interpret(AstList) ->
    interpret(AstList, []).

interpret(AstList, Macros) ->
    {ok,Re} = re:compile(<<"\\{\\{([^}]+)\\}\\}">>, [ungreedy]),
    State   = #interp{node=erlcfg_node:new(), macros=Macros, macro_re=Re},
    Scope   = '',
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


eval(#directive{name=schema, value=SchemaFile}, _Scope, #interp{schema_table=SchemaTable0}=State) ->
    SchemaFile1 = binary_to_list(SchemaFile),
    {ok, SchemaTable1} = erlcfg_schema:new(SchemaFile1),
    SchemaTable2 = erlcfg_schema:combine(SchemaTable0, SchemaTable1),
    State#interp{schema_table=SchemaTable2};

eval(#set{key=Key, value=Value}, Scope, #interp{}=State0) ->
    State1 = rhs(Value, State0),

    ScopedKey = erlcfg_node_addr:join([Scope, Key]),

    case erlcfg_node:set(State1#interp.node, ScopedKey, State1#interp.value) of
        {not_found, InvalidAddress} ->
            throw({not_found, InvalidAddress});
        NewNode ->
            State1#interp{node=NewNode}
    end;

eval(#block{name=Name, children=Children}, Scope, #interp{node=Node}=State0) ->
    ScopedName = erlcfg_node_addr:join([Scope, Name]),

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

rhs(#macro{name=Name}, #interp{macros=Map}=State) ->
    case maps:find(Name, Map) of
        {ok, Value} ->
            State#interp{value=Value};
        error ->
            throw({macro_not_found, Name})
    end;

rhs(#env{name=Name}, State) ->
    case os:getenv(Name) of
        Value when is_list(Value) ->
            State#interp{value = list_to_binary(Value)};
        false ->
            State#interp{value = <<>>}
    end;

rhs(#func{name='env',  arg=Env}, State) when is_atom(Env) ->
    rhs(#env{name=atom_to_list(Env)}, State);
rhs(#func{name='env',  arg=Env}, State) when is_list(Env) ->
    rhs(#env{name=Env}, State);
rhs(#func{name='date', arg=Date, opts=Opts}, State) when is_binary(Date) ->
    Utc = proplists:get_value(tz,  Opts, 'local'), % 'local' | 'utc'
    Now = to_now(proplists:get_value(now, Opts), Utc),
    State#interp{value = erlcfg_util:strftime(Date, Now, Utc)};
rhs(#func{name='path', arg=Path, opts=Opts}, State) when is_binary(Path) ->
    Utc = proplists:get_value(tz, Opts, 'local'), % 'local' | 'utc'
    Now = to_now(proplists:get_value(now, Opts), Utc),
    State#interp{value = erlcfg_util:pathftime(Path, Now, Utc)};
rhs(#func{name=Name, arg=Arg, opts=Opts}, _State) ->
    throw({unsupported_function, Name, [{arg=Arg, opts=Opts}]});

rhs(#list{data=nil}, State) ->
    State#interp{value=[]};

rhs(#list{data=Data}, State) ->
    State#interp{value=cons(Data, State)};

rhs(Data, State) when is_number(Data); is_atom(Data); is_boolean(Data) ->
    State#interp{value=Data};

rhs(Data, #interp{macros=Map, macro_re=Re} = State) when is_binary(Data) ->
    Data0 = erlcfg_util:strenv(Data),
    Data1 = replace_macros(Data0, Re, Map),
    State#interp{value=Data1};

rhs(Unknown, _State) -> % TODO: capture current scope?
    throw({unsupported_value_type, Unknown}).

replace_macros(Bin, Re, Map) ->
    case re:run(Bin, Re, [global, {capture, all, binary}]) of
    {match, L} ->
        lists:foldl(fun([MacroFull, MacroName], Val) ->
            try
                Macro = binary_to_existing_atom(MacroName, latin1),
                V     = maps:get(Macro, Map),
                re:replace(Val, MacroFull, to_bin(V), [global,{return,binary}])
            catch _:_ ->
                throw({undefined_macro, {MacroFull, Val}})
            end
        end, Bin, L);
    _ ->
        Bin
    end.

cons(#cons{head=Head, tail=nil}, State) ->
    NewState = rhs(Head, State),
    [NewState#interp.value];
cons(#cons{head=Head, tail=Tail}, State) ->
    NewState = rhs(Head, State),
    [NewState#interp.value | cons(Tail, NewState)].

to_now(undefined, _) -> erlang:timestamp();
to_now(Format,  Utc) when is_binary(Format), byte_size(Format) =:= 10 ->
    erlcfg_util:strptime(Format, <<"%Y-%m-%d">>, Utc);
to_now(Format,  Utc) when is_binary(Format), byte_size(Format) =:= 19 ->
    erlcfg_util:strptime(Format, <<"%Y-%m-%d %H:%M:%S">>, Utc).

to_bin(B) when is_binary(B)  -> B;
to_bin(B) when is_list(B)    -> list_to_binary(B);
to_bin(B) when is_atom(B)    -> atom_to_binary(B, latin1);
to_bin(B) when is_integer(B) -> integer_to_binary(B);
to_bin(B) when is_float(B)   -> float_to_binary(B);
to_bin(B)                    -> throw({cannot_convert_to_binary, B}).
