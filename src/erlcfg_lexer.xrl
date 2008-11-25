Definitions.

DIGIT = [0-9]
LETTER = [A-Za-z]
SYLLABLE_SEPERATOR = (_|-)
ALPHANUM = ({LETTER}|{DIGIT}|{SYLLABLE_SEPERATOR})
PUNCTUATION = ({SYLLABLE_SEPERATOR}|\~|\`|\!|\@|\#|\$|\%|\^|\&|\*|\(|\)|\+|\=|\{|\}|\[|\]|\:|\;|\||\\|\<|\>|\,|\.|\?|\/|\s|\n|\t)
PUNCT_ATOM = ({PUNCTUATION}|"|\\')
PUNCT_STRING = ({PUNCTUATION}|'|\\")
WS = ([\000-\s]|#.*)


Rules.
(\+|-)?{DIGIT}+ : 
    {token, {integer, TokenLine, list_to_integer(TokenChars)}}.

(\+|-)?{DIGIT}+(\.{DIGIT}+([e|E](\+|-)?{DIGIT}+)?)? : 
    {token, {float, TokenLine, list_to_float(TokenChars)}}.

(true|false) :
    {token, {bool, TokenLine, list_to_atom(TokenChars)}}.

({LETTER}|_){ALPHANUM}* : 
    {token, {atom, TokenLine, list_to_atom(TokenChars)}}.

\$({LETTER}|_){ALPHANUM}*(\.({LETTER}|_){ALPHANUM}*)* : 
    {token, {variable, TokenLine, list_to_atom(peell(TokenChars, TokenLen, 1))}}.

'({ALPHANUM}|{PUNCT_ATOM})*' : 
    {token, {quoted_atom, TokenLine, list_to_atom(peel(TokenChars, TokenLen, 1))}}.

"({ALPHANUM}|{PUNCT_STRING})*" :
    {token, {string, TokenLine, list_to_binary(peel(TokenChars, TokenLen, 1))}}.


= :
    {token, {'=', TokenLine}}.

; :
    {token, {';', TokenLine}}.

\( :
    {token, {'(', TokenLine}}.

\) :
    {token, {')', TokenLine}}.

\{ :
    {token, {'{', TokenLine}}.

\} :
    {token, {'}', TokenLine}}.


\, :
    {token, {',', TokenLine}}.

{WS}+ : 
    skip_token.

Erlang code.

peel(List, ListLen, Depth) ->
    lists:sublist(List, Depth+1, ListLen - (Depth+1)).

peell(List, ListLen, Depth) ->
    lists:sublist(List, Depth+1, ListLen).
