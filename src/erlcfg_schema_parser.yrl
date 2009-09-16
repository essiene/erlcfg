Nonterminals
schema items item typedef typedef_options data block block_contents block_data declaration type_signature list elements element.

Terminals
keyword_type datatype integer float atom quoted_atom string bool '=' ';' '{' '}' '[' ']' '|' ',' '(' ')'.

Rootsymbol schema.

schema -> '$empty' : [].
schema -> items : '$1'.

items -> item : ['$1'].
items -> item items: ['$1' | '$2'].

item -> typedef : '$1'.
item -> block : '$1'.
item -> declaration : '$1'.

typedef -> keyword_type atom '=' typedef_options ';' : {typedef, get_value('$2'), '$4'}.
block -> atom '{' block_contents '}' : {block, get_value('$1'), '$3'}.
block -> atom '{' '}' : {block, get_value('$1'), []}.
declaration -> type_signature atom ';' : {declaration, '$1', get_value('$2'), ?ERLCFG_SCHEMA_NIL}.
declaration -> type_signature atom '=' data ';' : {declaration, '$1', get_value('$2'), '$4'}.

block_contents -> block_data : ['$1'].
block_contents -> block_data block_contents : ['$1' | '$2'].

block_data -> block : '$1'.
block_data -> declaration : '$1'.

typedef_options -> data : {cons, '$1', nil}.
typedef_options -> data '|' typedef_options : {cons, '$1', '$3'}.

type_signature -> datatype : get_value('$1').
type_signature -> atom : get_value('$1').
type_signature -> '[' type_signature ']' : list_of_type('$2').

data -> list       : '$1'.
data -> integer    : get_value('$1').
data -> float      : get_value('$1').
data -> atom       : get_value('$1').
data -> quoted_atom: get_value('$1').
data -> string     : get_value('$1').
data -> bool       : get_value('$1').

list -> '(' elements ')' : '$2'.
elements -> element ',' elements    : {cons, '$1', '$3'}.
elements -> element     : {cons, '$1', nil}.
elements -> '$empty' : nil.
element -> data : '$1'.

Erlang code.
%nothing
-include("schema.hrl").

get_value({_Type, _Line, Value}) ->
    Value.

list_of_type(Type) ->
    #listof{type=Type}.
