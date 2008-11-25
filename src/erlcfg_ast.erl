-module(erlcfg_ast).
-export([
        traverse/1
    ]).


traverse(Ast) ->
    traverse(Ast, erlcfg_interp:new()).

traverse([], Interp) ->
    Interp;

traverse([Command | Rest], Interp) ->
    {NewInterp, _Value} = erlcfg_interp:eval(Interp, Command),
    traverse(Rest, NewInterp).
