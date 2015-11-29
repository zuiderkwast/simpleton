Terminals
boolean string
'~' '=' ident ';' '(' ')' '{' '}'
'[' ']' '@' ','
'if' then else
case of '->' do.

Nonterminals
% prog
module def defs
var expr literal
prefix postfix primary_expr exprs exprseq
rule rules.

Right 10 ';'.
Left  15 ','.
Right 15 '->'.
Right 20 else.
Right 30 '='.
Left  60 '~' '@'.

%% Old style: prog = a list of expr on top level
% Rootsymbol prog.
%% Program :: expr() (implicit do {...} around the whole program)
%prog -> '{' exprseq '}'   : {_, Pos} = '$1',
%                            mkexpr(#do{expr = '$2'}, Pos).

%% Future plan:
Rootsymbol module.
module -> '{' defs '}'    : #module{defs = '$2'}.
defs -> def defs          : {Name, Fun} = '$1',
                            orddict:update(Name,
                                           fun(Funs) -> #funcons{head = Fun,
                                                                 tail = Funs}
                                           end,
                                           #funcons{head = Fun},
                                           '$2').
defs -> '$empty'          : orddict:new().
def -> ident '(' exprs ')'
       '=' expr           : {ident, _Pos, Name} = '$1',
                            {Params, NumParams} = '$3',
                            Fun = #'fun'{params = Params,
                                         numparams = NumParams,
                                         body = '$6'},
                            {Name, Fun}.
                            %Clauses = #funcons{head = Fun},
                            %#def{name = Name, clauses = Clauses}.

%% Expressions :: #expr{}
expr -> prefix            : '$1'.

%% Array body
%% exprs :: {exprs(), Length::integer()}
exprs -> expr             : {#cons{head = '$1', tail = nil}, 1}.
exprs -> expr ',' exprs   : {TailCons, TailLen} = '$3',
                            {#cons{head = '$1', tail = TailCons}, 1 + TailLen}.

%% Array expr :: #expr{}
expr -> '[' exprs ']'     : {'[', Pos} = '$1',
                            {Cons, Len} = '$2',
                            mkexpr(#array{length = Len, elems = Cons}, Pos).
expr -> '[' ']'           : {'[', Pos} = '$1',
                            mkexpr(#array{length = 0, elems = nil}, Pos).

expr -> expr '~' expr     : {'~', Pos} = '$2',
                            mkexpr(#strcat{left = '$1', right = '$3'}, Pos).
expr -> expr '@' expr     : {'@', Pos} = '$2',
                            mkexpr(#arrcat{left = '$1', right = '$3'}, Pos).

% do { ... } sequence
expr -> do '{' exprseq '}'  : {do, Pos} = '$1',
                              mkexpr(#do{expr = '$3'}, Pos).
exprseq -> expr             : '$1'.
exprseq -> expr ';' exprseq : mkbinop(seq, '$1', '$3', '$2').

% Assignment/matching
expr -> expr '=' expr     : {'=', Pos} = '$2',
                             mkexpr(#assign{pat = '$1', expr = '$3'}, Pos).

expr -> if expr
        then expr
        else expr         : {'if', Pos} = '$1',
                            If = #'if'{'cond' = '$2',
                                       'then' = '$4',
                                       'else' = '$6'},
                            mkexpr(If, Pos).

% case TEST of RULES
expr -> 'case' expr 'of'
        '{' rules '}'     : {'case', Pos} = '$1',
                            mkexpr(#'case'{test = '$2', rules = '$5'}, Pos).
rules -> rule             : #rulecons{head = '$1', tail = nil}.
rules -> rule ';' rules   : #rulecons{head = '$1', tail = '$3'}.
rule -> expr '->' expr    : % {'->', Pos} = '$2',
                            #rule{pat = '$1', expr = '$3'}.

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
var -> ident              : {ident, Pos, Name} = '$1',
                            mkexpr(#var{name = Name}, Pos).

literal -> boolean        : mkliteral('$1').
literal -> string         : mkliteral('$1').


Erlang code.

-include("types.hrl").

%% Expr creation helpers

-spec mkexpr(exprbody(), integer()) -> #expr{}.
mkexpr(Body, {Line, _Col}) ->
	#expr{body = Body, line = Line}.

-spec mkbinop(atom(), #expr{}, #expr{}, {atom(), integer(), integer()}) ->
	#expr{}.
mkbinop(Op, Left, Right, OpToken) ->
	{_, Pos} = OpToken,
	mkexpr(#binop{op = Op, left = Left, right = Right}, Pos).

-spec mkliteral({typename(), any(), integer(), integer()}) -> #expr{}.
mkliteral(Token) ->
	{Type, Pos, Data} = Token,
	mkexpr(#literal{type = Type, data = Data}, Pos).
