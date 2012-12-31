%% Types
-type typename() :: integer | float | number | boolean | string | array | any.

%% Variable scope, plain and nested
-type scope()        :: [{string(), typename()}].
-type nested_scope() :: [scope()].

%% Annotated AST
-type accesstype() :: bind | access | lastaccess.

-record(var,     {name :: string(),
                  action = unknown :: accesstype() | unknown}).
-record(literal, {type :: string | boolean, data :: any()}).
-record(strcat,  {left :: expr(), right :: expr(),
                  fixed = unknown :: boolean() | unknown}).
-record(arrcat,  {left :: expr(), right :: expr(),
                  fixed = unknown :: boolean() | unknown}).
-record(binop,   {op :: seq, left :: expr(), right :: expr()}).
-record(assign,  {pat :: expr(), expr :: expr()}).
-record('if',    {'cond' :: expr(), 'then' :: expr(), 'else' :: expr()}).
-record(array,   {length :: integer(), elems :: exprs()}).

-record(cons, {head     :: expr(),
               tail     :: exprs(),
               accessed :: lsrvarsets:varset()}).
-type exprs() :: #cons{} | nil.

-type exprbody() :: #var{} | #literal{} | #strcat{} | #arrcat{} | #binop{} |
                    #assign{} | #'if'{} | #array{}.

-record(expr, {body               :: exprbody(),
               line               :: integer(),
               type     = unknown :: typename() | unknown,
               accessed = unknown :: lsrvarsets:varset() | unknown}).

-type expr() :: #expr{}.

%% @doc Annotated program tree
-record(prog, {body               :: expr(),
               type     = unknown :: typename() | unknown,
               locals   = unknown :: scope() | unknown,
               accessed = unknown :: lsrvarsets:varset() | unknown}).
