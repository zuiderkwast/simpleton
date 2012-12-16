Definitions.



Rules.

"[^"\\]*"  : {token, {string, TokenLine, TokenChars}}.
true|false : {token, {boolean, TokenLine, list_to_atom(TokenChars)}}.

if|then|else : {token, {list_to_atom(TokenChars),  TokenLine}}.

[a-zA-Z][a-zA-Z0-9_]* : {token, {ident, TokenLine, TokenChars}}.

\+\+    : {token, {'++', TokenLine}}.
\+      : {token, {'+', TokenLine}}.
\*      : {token, {'*', TokenLine}}.
=       : {token, {'=', TokenLine}}.
;       : {token, {';', TokenLine}}.
\(      : {token, {'(', TokenLine}}.
\)      : {token, {')', TokenLine}}.

\[      : {token, {'[', TokenLine}}.
\]      : {token, {']', TokenLine}}.
,       : {token, {',', TokenLine}}.

[\s\n\r\t] : skip_token.

Erlang code.