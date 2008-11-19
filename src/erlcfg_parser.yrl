Nonterminals
assignments assignment key value data.

Terminals 
integer float atom string '=' ';'.

Rootsymbol assignments.

assignments -> assignment ';' assignments : ['$1', '$3'].
assignments -> assignment : ['$1'].
assignments -> '$empty' : [].

assignment  -> key '=' value : {set, '$1', '$3'}.

key ->  atom        : get_value('$1').
value -> data       : {val, '$1'}. 

data -> integer    : get_value('$1').
data -> float      : get_value('$1').
data -> atom       : get_value('$1').
data -> string     : get_value('$1').

Erlang code.
%nothing

get_value({_Type, _Line, Value}) ->
    Value.
