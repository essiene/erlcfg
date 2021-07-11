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

Definitions.

DIGIT        = [0-9]
LETTER       = [A-Za-z]
INT_SUFFIX   = [GMKgmk]
SYLLABLE_SEP = (_|-)
ALPHANUM     = ({LETTER}|{DIGIT}|{SYLLABLE_SEP})
PUNCTUATION  = ({SYLLABLE_SEP}|\~|\`|\!|\@|\#|\$|\%|\^|\&|\*|\(|\)|\+|\=|\{|\}|\[|\]|\:|\;|\||\\|\<|\>|\,|\.|\?|\/|\s|\n|\t)
PUNCT_ATOM   = ({PUNCTUATION}|"|\\')
PUNCT_STRING = ({PUNCTUATION}|'|\\")
WS           = ([\000-\s]|#.*)


Rules.
(\+|-)?{DIGIT}+ : 
    {token, {integer, TokenLine, list_to_integer(TokenChars)}}.

(\+|-)?{DIGIT}+{INT_SUFFIX} : 
    {token, {integer, TokenLine, to_integer(TokenChars, TokenLen)}}.

(\+|-)?{DIGIT}+(\.{DIGIT}+([e|E](\+|-)?{DIGIT}+)?)? : 
    {token, {float, TokenLine, list_to_float(TokenChars)}}.

(true|false) :
    {token, {bool, TokenLine, list_to_atom(TokenChars)}}.

(int|float|atom|string|bool) :
    {token, {datatype, TokenLine, list_to_atom(TokenChars)}}.

type :
    {token, {keyword_type, TokenLine, list_to_atom(TokenChars)}}.

({LETTER}|_){ALPHANUM}* : 
    {token, {atom, TokenLine, list_to_atom(TokenChars)}}.

{UPPER}{ALPHANUM}* : 
    {token, {var, TokenLine, TokenChars}}.

\$\(({LETTER}|_){ALPHANUM}*\) :
    {token, {macro, TokenLine, list_to_atom(peel(TokenChars, TokenLen, 2))}}.

\$\{({LETTER}|_){ALPHANUM}*\} :
    {token, {env, TokenLine, list_to_binary(peel(TokenChars, TokenLen, 2))}}.

\$({LETTER}|_){ALPHANUM}*(\.({LETTER}|_){ALPHANUM}*)* : 
    {token, {variable, TokenLine, list_to_atom(peel(TokenChars, TokenLen, 1, 0))}}.

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

\[ :
    {token, {'[', TokenLine}}.

\] :
    {token, {']', TokenLine}}.

\| :
    {token, {'|', TokenLine}}.

\, :
    {token, {',', TokenLine}}.

{WS}+ : 
    skip_token.

Erlang code.

peel(List, ListLen, StripH) ->
    peel(List, ListLen, StripH, 1).

peel(List, ListLen, StripH, StripT) when is_list(List) ->
    lists:sublist(List, StripH+1, ListLen - (StripH+StripT)).

to_integer(List, Len) ->
    {L, [Sfx]} = lists:split(Len-1, List),
    I = list_to_integer(L),
    case Sfx of
    $G -> I * 1073741824;
    $M -> I * 1048576;
    $K -> I * 1024;
    $g -> I * 1000000000;
    $m -> I * 1000000;
    $k -> I * 1000
    end.
