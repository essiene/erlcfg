Nonterminals
assignments assignment key value data.

Terminals 
integer float atom quoted_atom string bool variable '=' ';' '{' '}'.

Rootsymbol assignments.

assignments -> assignment ';' assignments : ['$1', '$3'].
assignments -> assignment : ['$1'].
assignments -> '$empty' : [].

assignment -> key '=' '{' assignments '}' : [{block, '$1', noop}, '$4', {endblock, noop, noop}].
assignment  -> key '=' value : {set, '$1', '$3'}.

key ->  atom        : get_value('$1').
value -> data       : {val, '$1', noop}. 

data -> integer    : get_value('$1').
data -> float      : get_value('$1').
data -> atom       : get_value('$1').
data -> quoted_atom: get_value('$1').
data -> string     : get_value('$1').
data -> bool       : get_value('$1').
data -> variable   : {get, get_value('$1'), noop}.

Erlang code.
%nothing

get_value({_Type, _Line, Value}) ->
    Value.
