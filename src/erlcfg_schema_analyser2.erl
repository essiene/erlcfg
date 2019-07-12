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

-module(erlcfg_schema_analyser2).
-export([analyse/2, analyse/3]).
-include("schema.hrl").

% The purpose of this stage is to ensure that we are not using any types in the
% schema definition that we have not defined, and then to generate a  node address
% to type mapping which looks like:
%
% #{
%    {node1.intkey1, #declaration{default=?ERLCFG_SCHEMA_NIL,
%                                 validator=#validator{name=integer, 
%                                                      test=fun is_integer/1}}
%    },
%    {node1.stringkey1, #declaration{default = <<"defaultvalue">>,
%                                    validator = #validator{name=string, 
%                                                           test=fun is_binary/1}}
%    },
%    {node2.intkey2, #declaration{default=5,
%                                 validator=#validator{name=integer, 
%                                                      test=fun is_integer/1}}
%    },
%    {node2.stringkey2, #declaration{default=?ERLCFG_SCHEMA_NIL,
%                                    validator=#validator{name=string, 
%                                                         test=fun is_binary/1}}
%    },
%    {truefalse, #declaration{default=false,
%                             validator=#validator{name=boolean, 
%                                                  test=fun is_boolean/1}}
%    }
% }

analyse(Ast, Types) ->
    analyse(Ast, Types, #{}).

analyse(Ast, Types, Macros) when is_list(Ast), is_list(Types), is_map(Macros) ->
    Res = analyse2(Ast, Types, Macros),
    maps:from_list(Res).

analyse2([], _Types, _Macros) ->
    [];
analyse2([Head|Rest], Types, Macros) ->
    analyse(Head, Rest, '', [], Types, Macros).

analyse(Current, [], Scope, Accm, Types, Macros) ->
    process(Current, Scope, Accm, Types, Macros);
analyse(Current, [Head|Rest], Scope, Accm, Types, Macros) ->
    Accm0 = process(Current, Scope, Accm, Types, Macros),
    analyse(Head, Rest, Scope, Accm0, Types, Macros).

process(#block{name=Name,child=[Head|Rest]}, Scope, Accm, Types, Macros) ->
    Scope0 = erlcfg_node_addr:join([Scope, Name]),
    analyse(Head, Rest, Scope0, Accm, Types, Macros);
process(D = #declaration{type=#listof{type=DeclType}, name=Name, attrs=Attrs}, Scope, Accm, Types, Macros) ->
    Addr = erlcfg_node_addr:join([Scope, Name]),
    ok   = check_is_already_defined(Addr, Accm),
    {ok, Validator} = check_has_known_type(DeclType, Types),

    Fun  = Validator#validator.test,
    ListFun = fun(Val) ->
            Val2 = lists:filter(Fun, Val),
            Val == Val2
    end,

    Val  = Validator#validator{test=ListFun},
    D1   = D#declaration{attrs = to_attrs(DeclType, Name, Attrs, Val, Macros), validator=Val},
    [{Addr, D1}|Accm];
process(D = #declaration{type=DeclType, name=Name, attrs=Attrs}, Scope, Accm, Types, Macros) ->
    Addr = erlcfg_node_addr:join([Scope, Name]),
    ok   = check_is_already_defined(Addr, Accm),
    {ok, Val} = check_has_known_type(DeclType, Types),
    D1   = D#declaration{attrs = to_attrs(DeclType, Name, Attrs, Val, Macros), validator=Val},
    [{Addr, D1}|Accm];
process(_Other, _Scope, Accm, _Types, _Macros) ->
    Accm.

rhs(#macro{name=Name}, Type, Macros) ->
    case maps:find(Name, Macros) of
        {ok, Value} -> to_type(Type, Value);
        error       -> throw({macro_not_found, Name})
    end;
rhs(#env{name=Name}, Type, _Macros) ->
    case os:getenv(to_list(Name)) of
        Value when is_list(Value) ->
            to_type(Type, Value);
        false ->
            ?ERLCFG_SCHEMA_NIL
    end;
rhs(Other, _Type, _Macros) ->
    Other.

to_list(V) when is_binary(V) -> binary_to_list(V);
to_list(V) when is_list(V)   -> V;
to_list(V) when is_atom(V)   -> atom_to_list(V).

to_attrs(Type, Name, Attrs, Validator, Macros) ->
    lists:foldl(fun
        ({default, D}, R) ->
            Value0 = rhs(D, Type, Macros),
            {ok, Default} = check_typeof_default(Value0, Validator, R),
            R#attrs{default = Default};
        ({min, N}, R) when is_integer(N); is_float(N) ->
            R#attrs{min = N};
        ({max, N}, R) when is_integer(N); is_float(N) ->
            R#attrs{max = N};
        ({unique, N}, R) when is_boolean(N) ->
            R#attrs{unique = N};
        ({nullable, V}, R) when is_boolean(V) ->
            R#attrs{nullable = V};
        ({null, V}, R) ->
            R#attrs{nullable = true, null = V};
        (Other, _R) ->
            throw({invalid_attribute, Name, Other})
    end, #attrs{}, Attrs).

check_is_already_defined(Addr, Accm) ->
    case proplists:get_value(Addr, Accm) of
        undefined ->
            ok;
        _Found ->
            {error, {type_already_defined_in_scope, Addr}}
    end.

check_has_known_type(DeclType, Types) ->
    case proplists:get_value(DeclType, Types) of
        undefined ->
            {error, {unknown_type, DeclType}};
        Validator ->
            {ok, Validator}
    end.

check_typeof_default(?ERLCFG_SCHEMA_NIL, _Validator, _Attrs) ->
    {ok, ?ERLCFG_SCHEMA_NIL};
check_typeof_default(#cons{}=Cons, Validator, Attrs) ->
    {ok, Cons0} = cons(Cons, []),
    check_typeof_default(Cons0, Validator, Attrs);
check_typeof_default(Value, Validator, #attrs{nullable=Nullable, null=Null}) ->
    Fun = Validator#validator.test,
    case Fun(Value) of
        true ->
            {ok, Value};
        false when Nullable, Value =:= Null ->
            {ok, Value};
        false ->
            {error, 
                {invalid_default_value, Value, {expected, Validator#validator.type}}
            }
    end.

cons(#cons{head=Head, tail=nil}, Accm) ->
    Accm0 = [Head|Accm],
    {ok, lists:reverse(Accm0)};
cons(#cons{head=Head, tail=Tail}, Accm) ->
    Accm0 = [Head|Accm],
    cons(Tail, Accm0).

to_type(Type, Value) when Type =:= string ->
    if is_list(Value) ->
        list_to_binary(Value);
    is_binary(Value) ->
        Value
    end;
to_type(Type, Value) when Type =:= integer ->
    list_to_integer(Value);
to_type(Type, Value) when Type =:= float ->
    try list_to_float(Value)
    catch error:badarg ->
        float(list_to_integer(Value))
    end;
to_type(Type, Value) when Type =:= atom ->
    list_to_atom(Value);
to_type(Type, Value) when Type =:= list, is_list(Value) ->
    Value.
