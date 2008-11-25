-module(erlcfg_ast).
-export([
        traverse/1
    ]).


traverse(Ast) ->
    traverse(Ast, erlcfg_interp:new()).

traverse([], Interp) ->
    Interp;

traverse([Command | Rest], Interp) when is_tuple(Command) ->
    {NewInterp, _Value} = erlcfg_interp:eval(Interp, Command),
    traverse(Rest, NewInterp);

traverse([Ast | Rest], Interp) when is_list(Ast) ->
    NewInterp = traverse(Ast, Interp),
    traverse(Rest, NewInterp).
