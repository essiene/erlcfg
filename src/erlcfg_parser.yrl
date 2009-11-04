Nonterminals
config items item assignment block key value data list elements element
directive directives directive_name directive_value.

Terminals 
integer float atom quoted_atom string bool variable '=' ';' '{' '}' '(' ')' ',' '@'.

Rootsymbol config.

config -> '$empty' : [].
config -> items : '$1'.
config -> directives : '$1'.
config -> directives items : lists:append(['$1', '$2']).

directives -> directive : ['$1'].
directives -> directive directives : ['$1' | '$2'].

directive -> '@' directive_name '(' directive_value ')' ';' : {directive, '$2', '$4'}.

directive_name -> atom : get_value('$1').

directive_value -> data : '$1'.

items -> item : ['$1'].
items -> item items: ['$1' | '$2'].

item -> block : '$1'.
item -> assignment : '$1'.

block -> key '{' config '}' : {block, '$1', '$3'}.

assignment -> key '=' value ';' : {set, '$1', '$3'}.

key ->  atom        : get_value('$1').
value -> data       : '$1'.
value -> list       : '$1'.

data -> integer    : get_value('$1').
data -> float      : get_value('$1').
data -> atom       : get_value('$1').
data -> quoted_atom: get_value('$1').
data -> string     : get_value('$1').
data -> bool       : get_value('$1').
data -> variable   : {get, get_value('$1')}.

list -> '(' elements ')' : {list, '$2'}.
elements -> element ',' elements    : {cons, '$1', '$3'}.
elements -> element     : {cons, '$1', nil}.
elements -> '$empty' : nil.
element -> value : '$1'.

Erlang code.
%nothing
-include("erlcfg.hrl").

get_value({_Type, _Line, Value}) ->
    Value.
