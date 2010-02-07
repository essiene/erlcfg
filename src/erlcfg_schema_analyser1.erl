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

