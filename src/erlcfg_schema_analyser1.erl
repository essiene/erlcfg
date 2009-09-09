-module(erlcfg_schema_analyser1).
-include("schema.hrl").
-export([analyse/1]).

analyse([]) ->
    [integer, float, atom, string, bool];
analyse([Head|Rest]) ->
    Types = [integer, float, atom, string, bool],
    analyse(Head, Rest, Types).

analyse(Current, [Head|Rest], Types0) ->
    Types1 = eval(Current, Types0),
    analyse(Head, Rest, Types1);

analyse(Current, [], Types0) ->
    Types1 = eval(Current, Types0),
    Types1.

eval(#typedef{name=Name, options=OptionAst}, Types) ->
    Options = cons(OptionAst),
    [{Name, Options}|Types];
eval(_Other, Types) ->
    Types.

cons(#cons{}=Cons) ->
    cons(Cons, []).

cons(#cons{head=Head, tail=nil}, Accm) ->
    lists:reverse([Head|Accm]);
cons(#cons{head=Head, tail=Tail}, Accm) ->
    cons(Tail, [Head|Accm]).

