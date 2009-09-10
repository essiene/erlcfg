-module(erlcfg_schema_analyser1).
-include("schema.hrl").
-export([analyse/1]).

% The result of this transform on the AST is a mapping like
% shown below:
% 
% [
%        {'CustomType1', #validator{name='CustomType1', 
%                                  test=CustomType1Fun}
%        },
%        {customtype2, #validator{name=customtype2,
%                                 test=CustomeType2Fun}
%        },
%        {integer, #validator{type=integer, 
%                             test=fun is_integer/1}
%        },
%        {float, #validator{type=float, 
%                          test=fun is_float/1}
%        },
%        {atom, #validator{type=atom, 
%                          test=fun is_atom/1}
%        },
%        {string, #validator{type=string, 
%                            test=fun is_binary/1}
%        },
%        {boolean, #validator{type=boolean, 
%                             test=fun is_boolean/1}
%        }
% ]


analyse([]) ->
    {ok, ?DEFAULT_TYPE_MAP};
analyse([Head|Rest]) ->
    Types = ?DEFAULT_TYPE_MAP,
    {ok, analyse(Head, Rest, Types)}.

analyse(Current, [Head|Rest], Types0) ->
    Types1 = eval(Current, Types0),
    analyse(Head, Rest, Types1);

analyse(Current, [], Types0) ->
    Types1 = eval(Current, Types0),
    Types1.

eval(#typedef{name=Name, options=OptionAst}, Types) ->
    Options = cons(OptionAst),
    Fun = fun(Val) ->
            lists:member(Val, Options)
    end,
    TypeTest = #validator{type=Name, test=Fun},
    [{Name, TypeTest}|Types];
eval(_Other, Types) ->
    Types.

cons(#cons{}=Cons) ->
    cons(Cons, []).

cons(#cons{head=Head, tail=nil}, Accm) ->
    lists:reverse([Head|Accm]);
cons(#cons{head=Head, tail=Tail}, Accm) ->
    cons(Tail, [Head|Accm]).

