Terminals
boolean string
'++' '=' ident ';' '(' ')'
'[' ']' ','
'if' then else.

Nonterminals
var expr prog literal
assign seqexpr prefix postfix primary_expr exprs.

Rootsymbol prog.

Right 10 ';'.
Left  15 ','.
Right 20 else.
Right 30 '='.
Left  60 '++'.

% Program
prog -> expr                        : {prog, '$1'}.

% Expressions
expr -> prefix                      : '$1'.

% Array
% Store total length, and a sequence of arraycons, nil-terminated
exprs -> expr                       : {{arraycons, '$1', nil}, 1}.
exprs -> expr ',' exprs             : {TailCons, TailLen} = '$3',
                                      {{arraycons, '$1', TailCons}, 1+TailLen}.
expr -> '[' exprs ']'               : {Cons, Len} = '$2',
                                      {array, Len, Cons}.
expr -> '[' ']'                     : {array, 0, nil}.

expr -> expr '++' expr              : {concat, '$1', '$3'}.

% Seq as a binary op
expr -> seqexpr ';' expr            : {seq, '$1', '$3'}.

% Assignment, like let-in, always followed by an expr
assign -> expr '=' expr             : {assign, '$1', '$3'}.
seqexpr -> assign                   : '$1'.
seqexpr -> expr                     : '$1'.

expr -> if expr then expr else expr : {'if', '$2', '$4', '$6'}.

% Prefix and postfix stuff (juxtaposition)
prefix -> postfix                   : '$1'.
prefix -> '[' exprs ']' prefix      : {concat, '$1', '$3'}.
%prefix -> literal prefix            : {concat, '$1', '$2'}.

postfix -> primary_expr             : '$1'.
postfix -> postfix '[' exprs ']'    : {concat, '$1', '$3'}.
%postfix -> postfix literal          : {concat, '$1', '$2'}.

primary_expr -> var                 : '$1'.
primary_expr -> literal             : '$1'.
primary_expr -> '(' expr ')'        : '$2'.

var -> ident                        : {ident, Line, Name} = '$1',
                                      {var, Line, Name}.

literal -> boolean                  : {literal, '$1'}.
literal -> string                   : {literal, '$1'}.


Erlang code.

%% Binary operations for expressions
-type binop() :: concat | seq.

-type literal() :: {string, integer(), string()}
                 | {boolean, integer(), boolean()}.

-type arraycons() :: {arraycons, expr(), arraycons()} | nil.

-type expr() :: {var, integer(), string()}
              | {literal, literal()}
              | {binop(), expr(), expr()}
              | {assign, string(), expr()}
              | {array, Length::integer(), Body::arraycons()}.

-type prog() :: {prog, expr()}.

-export_type([literal/0, binop/0, expr/0, prog/0]).
