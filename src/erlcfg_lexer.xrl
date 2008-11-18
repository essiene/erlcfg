Definitions.

DIGIT = [0-9]
LETTER = [A-Za-z]
WS = ([\000-\s]|#.*)


Rules.
(\+|-)?{DIGIT}+  : {token, {integer, TokenLine, list_to_integer(TokenChars)}}.
(\+|-)?{DIGIT}+(\.{DIGIT}+([e|E](\+|-)?{DIGIT}+)?)?    : {token, {float, TokenLine, list_to_float(TokenChars)}}.
{WS}+   : skip_token.

Erlang code.
%nothing
