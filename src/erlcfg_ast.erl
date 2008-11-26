-module(erlcfg_ast).
-export([
        traverse/1
    ]).


traverse(Ast) ->
    traverse(Ast, '', erlcfg_interp:new()).

traverse([], _CurrentBlock, Interp) ->
    Interp;

traverse([Command | Rest], CurrentBlock, Interp) when is_tuple(Command) ->
    {NewInterp, NewBlock, _Value} = erlcfg_interp:eval(Interp, CurrentBlock, Command),
    traverse(Rest, NewBlock, NewInterp);

traverse([Ast | Rest], CurrentBlock, Interp) when is_list(Ast) ->
    NewInterp = traverse(Ast, CurrentBlock, Interp),
    traverse(Rest, CurrentBlock, NewInterp).
