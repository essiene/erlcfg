-module(erlcfg_schema_analyser2).
-export([analyse/2]).
-include("schema.hrl").

% The purpose of this stage is to ensure that we are not using any types in the
% schema definition that we have not defined, and then to generate a  node address
% to type mapping which looks like:
%
% [
%    {node1.intkey1, {?ERLCFG_SCHEMA_NIL, #validator{name=integer, 
%                               test=fun is_integer/1}}
%    },
%    {node1.stringkey1, {<<"defaultvalue">>, #validator{name=string, 
%                                  test=fun is_binary/1}}
%    },
%    {node2.intkey2, {5, #validator{name=integer, 
%                               test=fun is_integer/1}}
%    },
%    {node2.stringkey2, {?ERLCFG_SCHEMA_NIL, #validator{name=string, 
%                                  test=fun is_binary/1}}
%    },
%    {truefalse, {false, #validator{name=boolean, 
%                           test=fun is_boolean/1}}
%    }
% ]


analyse([], _Types) ->
    [];
analyse([Head|Rest], Types) ->
    analyse(Head, Rest, [], [], Types).

analyse(Current, [], Scope, Accm, Types) ->
    process(Current, Scope, Accm, Types);
analyse(Current, [Head|Rest], Scope, Accm, Types) ->
    Accm0 = process(Current, Scope, Accm, Types),
    analyse(Head, Rest, Scope, Accm0, Types).

process(#block{name=Name,child=[Head|Rest]}, Scope, Accm, Types) ->
    Scope0 = [Name | Scope],
    analyse(Head, Rest, Scope0, Accm, Types);
process(#declaration{type=#listof{type=DeclaredType}, name=Name, default=Default}, Scope, Accm, Types) ->
    Addr = build_node_addr(Name, Scope),
    ok = check_is_already_defined(Addr, Accm),
    {ok, Validator} = check_has_known_type(DeclaredType, Types),

    Fun = Validator#validator.test,
    ListFun = fun(Val) ->
            Val2 = lists:filter(Fun, Val),
            Val == Val2
    end,

    Validator0 = Validator#validator{test=ListFun},
    {ok, Default0} = check_typeof_default(Default, Validator0),

    [{Addr, {Default0, Validator0}}|Accm];
process(#declaration{type=DeclaredType, name=Name, default=Default}, Scope, Accm, Types) ->
    Addr = build_node_addr(Name, Scope),
    ok = check_is_already_defined(Addr, Accm),
    {ok, Validator} = check_has_known_type(DeclaredType, Types),
    {ok, Default0} = check_typeof_default(Default, Validator),
    [{Addr, {Default0, Validator}}|Accm];
process(_Other, _Scope, Accm, _Types) ->
    Accm.


build_node_addr(Name, Scope) ->
    Scope0 = lists:reverse([Name|Scope]),
    node_addr:join(Scope0).

check_is_already_defined(Addr, Accm) ->
    case proplists:get_value(Addr, Accm) of
        undefined ->
            ok;
        _Found ->
            {error, {type_already_defined_in_scope, Addr}}
    end.

check_has_known_type(DeclaredType, Types) ->
    case proplists:get_value(DeclaredType, Types) of
        undefined ->
            {error, {unknown_type, DeclaredType}};
        Validator ->
            {ok, Validator}
    end.

check_typeof_default(?ERLCFG_SCHEMA_NIL, _Validator) ->
    {ok, ?ERLCFG_SCHEMA_NIL};
check_typeof_default(#cons{}=Cons, Validator) ->
    {ok, Cons0} = build_cons(Cons, []),
    check_typeof_default(Cons0, Validator);
check_typeof_default(Value, Validator) ->
    Fun = Validator#validator.test,
    case Fun(Value) of
        true ->
            {ok, Value};
        false ->
            {error, 
                {invalid_default_value, Value, {expected, Validator#validator.type}}
            }
    end.

build_cons(#cons{head=Head, tail=nil}, Accm) ->
    Accm0 = [Head|Accm],
    {ok, lists:reverse(Accm0)};
build_cons(#cons{head=Head, tail=Tail}, Accm) ->
    Accm0 = [Head|Accm],
    build_cons(Tail, Accm0).

