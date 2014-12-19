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

Nonterminals
config items item assignments assignment block key value data strings list
elements element directive directives directive_name directive_value fun_args.

Terminals 
integer float atom quoted_atom string bool var variable macro env '=' ';' '{' '}' '(' ')' ',' '@'.

Rootsymbol config.

config -> '$empty' : [].
config -> items : '$1'.
config -> directives : '$1'.
config -> directives items : lists:append(['$1', '$2']).

directives -> directive : ['$1'].
directives -> directive directives : ['$1' | '$2'].

directive -> '@' directive_name '(' directive_value ')' ';' : {directive, '$2', '$4'}.
directive -> '@' directive_name '(' directive_value ')'     : {directive, '$2', '$4'}.

directive_name -> atom : get_value('$1').

directive_value -> data : '$1'.

items -> item : lists:append(['$1']).
items -> item items: lists:append(['$1', '$2']).

item -> block : ['$1'].
item -> assignments ';' : '$1'.
item -> assignments     : '$1'.

block -> key '{' config '}' : {block, '$1', '$3'}.

assignments -> assignment                 : ['$1'].
assignments -> assignment ',' assignments : ['$1' | '$3'].

% ';' or ',' is an optional separator
assignment -> key '=' value : {set, '$1', '$3'}.

key   -> atom      : get_value('$1').
value -> data      : '$1'.
value -> list      : '$1'.

data -> integer    : get_value('$1').
data -> float      : get_value('$1').
data -> atom       : get_value('$1').
data -> quoted_atom: get_value('$1').
data -> strings    : '$1'.
data -> bool       : get_value('$1').
data -> macro      : {macro, get_value('$1')}.
data -> env        : {env,   get_value('$1')}.
data -> variable '{' fun_args '}' :
                     {func, list_to_atom(atom_to_list(get_value('$1'))),
                            element(1, '$3'), element(2, '$3')}.
data -> variable   : {get,   get_value('$1')}.

fun_args  -> var                    : {list_to_atom(get_value('$1')), []}.
fun_args  -> string                 : {get_value('$1'), []}.
fun_args  -> var ',' assignments    : {list_to_atom(get_value('$1')), '$3'}.
fun_args  -> string ',' assignments : {get_value('$1'), '$3'}.

list -> '(' elements ')'            : {list, '$2'}.
elements -> element ',' elements    : {cons, '$1', '$3'}.
elements -> element                 : {cons, '$1', nil}.
elements -> '$empty'                : nil.
element  -> value                   : '$1'.

strings  -> string : get_value('$1').
strings  -> string strings : <<(get_value('$1'))/binary, ('$2')/binary>>.

Erlang code.
%nothing
-include("erlcfg.hrl").

get_value({_Type, _Line, Value}) ->
    Value.
