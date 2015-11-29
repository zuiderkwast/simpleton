%% Types
-type typename() :: integer | float | number | boolean | string | array | any.

%% Variable scope, plain and nested
-type scope()        :: [{string(), typename()}].
-type nested_scope() :: [scope()].

%% Annotated AST
-type accesstype() :: bind | discard | access | lastaccess.

%% Set of variable names
-type varset() :: ordsets:ordset(string()).

-record(var,     {name :: string(),
                  action = unknown :: accesstype() | unknown}).
-record(literal, {type :: string | boolean, data :: any()}).
-record(array,   {length :: integer(), elems :: exprs()}).
-record(strcat,  {left :: expr(), right :: expr(),
                  fixed = unknown :: boolean() | unknown}).
-record(arrcat,  {left :: expr(), right :: expr(),
                  fixed = unknown :: boolean() | unknown}).
-record(binop,   {op :: seq, left :: expr(), right :: expr()}).
-record(assign,  {pat :: expr(), expr :: expr()}).
-record('if',    {'cond' :: expr(), 'then' :: expr(), 'else' :: expr()}).
-record('case',  {test :: expr(), rules :: rules()}).
-record(do,      {expr             :: expr(),
                  locals = unknown :: scope() | unknown}).

%% exprs, the comma-separated contents of an array [ ... ]
-record(cons, {head               :: expr(),
               tail               :: exprs(),
               type     = unknown :: typename() | unknown, % not used yet
               accessed = unknown :: varset()}).
-type exprs() :: #cons{} | nil.

%% Case rules on the from Pattern -> Expression
-record(rule, {pat :: expr(), expr :: expr()}).
-record(rulecons, {head               :: rule(),
                   tail               :: rules(),
                   type     = unknown :: typename() | unknown, % not used yet
                   accessed = unknown :: varset()}).
-type rule() :: #rule{}.
-type rules() :: #rulecons{} | nil.

-type exprbody() :: #var{} | #literal{} | #strcat{} | #arrcat{} | #binop{} |
                    #assign{} | #'if'{} | #'case'{} | #array{} | #do{}.

-record(expr, {body               :: exprbody(),
               line               :: integer(),
               type     = unknown :: typename() | unknown,
               accessed = unknown :: varset() | unknown}).

-type expr() :: #expr{}.

%% @doc Annotated program tree (old)
-record(prog, {body               :: expr(),
               type     = unknown :: typename() | unknown,
               locals   = unknown :: scope() | unknown,
               accessed = unknown :: varset() | unknown}).

-record('fun', {params             :: exprs(), % patterns
                numparams          :: integer,
                body               :: expr(),
                locals   = unknown :: scope() | unknown,
                type     = unknown :: scope() | unknown,
                accessed = unknown :: varset() | unknown}).

%% nil-terminated list of #fun{}s, with annotations
-record(funcons, {head               :: #'fun'{},
                  tail     = nil     :: funs(),
                  type     = unknown :: typename() | unknown, % not used yet
                  accessed = unknown :: varset()}).

-type funs() :: #funcons{} | nil.

-record(module, {defs            :: orddict:orddict(), %% orddict with key=Name::string(), value::funs()
                 types = unknown :: scope() | unknown}).
