Terminals
boolean string
'~' '=' ident ';' '(' ')'
'[' ']' '@' ','
'if' then else.

Nonterminals
var expr prog literal
assign seqexpr prefix postfix primary_expr exprs.

Rootsymbol prog.

Right 10 ';'.
Left  15 ','.
Right 20 else.
Right 30 '='.
Left  60 '~' '@'.

%% Program :: #prog{}
prog -> expr              : #prog{body='$1'}.

%% Expressions :: #expr{}
expr -> prefix            : '$1'.

%% Array body
%% exprs :: {exprs(), Length::integer()}
exprs -> expr             : {#cons{head = '$1', tail = nil}, 1}.
exprs -> expr ',' exprs   : {TailCons, TailLen} = '$3',
                            {#cons{head = '$1', tail = TailCons}, 1 + TailLen}.

%% Array expr :: #expr{}
expr -> '[' exprs ']'     : {'[', Line} = '$1',
                            {Cons, Len} = '$2',
                            mkexpr(#array{length = Len, elems = Cons}, Line).
expr -> '[' ']'           : {'[', Line} = '$1',
                            mkexpr(#array{length = 0, elems = nil}, Line).

expr -> expr '~' expr     : {'~', Line} = '$2',
                            mkexpr(#strcat{left = '$1', right = '$3'}, Line).
expr -> expr '@' expr     : {'@', Line} = '$2',
                            mkexpr(#arrcat{left = '$1', right = '$3'}, Line).

% Seq as a binary op
expr -> seqexpr ';' expr  : mkbinop(seq, '$1', '$3', '$2').

% Assignment (like let-in, always followed by an expr)
assign -> expr '=' expr   : {'=', Line} = '$2',
                            mkexpr(#assign{pat = '$1', expr = '$3'}, Line).
seqexpr -> assign         : '$1'.
seqexpr -> expr           : '$1'.

expr -> if expr
        then expr
        else expr         : {'if', Line} = '$1',
                            If = #'if'{'cond' = '$2',
                                       'then' = '$4',
                                       'else' = '$6'},
                            mkexpr(If, Line).

%% Prefix and postfix stuff (juxtaposition)

%% prefix :: #expr{}
prefix -> postfix         : '$1'.
%prefix -> '[' exprs ']' prefix.

%% postfix :: #expr{}
postfix -> primary_expr   : '$1'.
%postfix -> postfix '[' exprs ']'.

%% primary_expr :: #expr{}
primary_expr -> var          : '$1'.
primary_expr -> literal      : '$1'.
primary_expr -> '(' expr ')' : '$2'.

%% Variables and literals :: #expr{}
var -> ident              : {ident, Line, Name} = '$1',
                            mkexpr(#var{name = Name}, Line).

literal -> boolean        : mkliteral('$1').
literal -> string         : mkliteral('$1').


Erlang code.

-include("types.hrl").

%% Expr creation helpers

-spec mkexpr(exprbody(), integer()) -> #expr{}.
mkexpr(Body, Line) ->
	#expr{body = Body, line = Line}.

-spec mkbinop(atom(), #expr{}, #expr{}, {atom(), integer()}) -> #expr{}.
mkbinop(Op, Left, Right, OpToken) ->
	{_, Line} = OpToken,
	mkexpr(#binop{op = Op, left = Left, right = Right}, Line).

-spec mkliteral({typename(), integer(), any()}) -> #expr{}.
mkliteral(Token) ->
	{Type, Line, Data} = Token,
	mkexpr(#literal{type = Type, data = Data}, Line).
