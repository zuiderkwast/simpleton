%% @doc Typechecker/inferer/annotator for the syntax tree produced by the parser.
%% Most of the functions check and assign annotations to an expression.
-module(lsrtyper).

-export([annotate/1, get_accessed/1, rules_get_accessed/1, get_type/1,
         is_subtype_of/2, is_fixed/1]).

-include("types.hrl").

%% Annotated AST

%% @doc Returns the type of an annotated expression.
-spec get_type(#expr{}) -> typename().
get_type(#expr{type = T}) -> T.

%% @doc Returns a list of variables accessed or maybe accessed by the
%% (annotated) expression.
-spec get_accessed(expr()) -> varset().
get_accessed(#expr{accessed = A}) -> A.

-spec exprs_get_accessed(exprs()) -> varset().
exprs_get_accessed(#cons{accessed = A}) -> A;
exprs_get_accessed(nil) -> ordsets:new().

-spec rules_get_accessed(rules()) -> varset().
rules_get_accessed(#rulecons{accessed = A}) -> A;
rules_get_accessed(nil) -> ordsets:new().

-spec is_subtype_of(typename(), typename()) -> boolean().
is_subtype_of(T, T) -> true;
is_subtype_of(integer, number) -> true;
is_subtype_of(float, number) -> true;
is_subtype_of(_, any) -> true;
is_subtype_of(_, _) -> false.

-spec get_parent_type(typename()) -> typename().
get_parent_type(integer) -> number;
get_parent_type(float)   -> number;
get_parent_type(_)       -> any.

%% @doc The "generality" level of the type in the type hierarchy.
%% High number is more general. Low number is more specific.
-spec get_type_generality(typename()) -> integer().
get_type_generality(any)    -> 3;
get_type_generality(number) -> 2;
get_type_generality(_)      -> 1.

%% @doc Unifies two types by finding the most specific common supertype.
-spec get_common_supertype(typename(), typename()) -> typename().
get_common_supertype(T, T) -> T;
get_common_supertype(T1, T2) ->
	G1 = get_type_generality(T1),
	G2 = get_type_generality(T1),
	if
		G1 == G2 ->
			get_common_supertype(get_parent_type(T1), get_parent_type(T2));
		G1 < G2 ->
			get_common_supertype(get_parent_type(T1), T2);
		G1 > G2 ->
			get_common_supertype(T1, get_parent_type(T2))
	end.

%% @doc A helper that throws an exception with a type mismatch message.
assert_type(Expected, Found, What, Line) ->
	case Found of
		Expected ->
			ok;
		_ ->
			throw(io_lib:format("Type mismatch: ~s. Found ~p, expected ~p, "
			                    "on line ~p.",
			                    [What, Found, Expected, Line]))
	end.

-spec scope_to_varset(scope()) -> varset().
scope_to_varset(Vars) ->
	scope_to_varset(Vars, []).
scope_to_varset([], Acc) ->
	ordsets:from_list(Acc);
scope_to_varset([{Name, _Type}|Vars], Acc) ->
	scope_to_varset(Vars, [Name | Acc]).

%% @doc Returns true is a pattern expression is "fixed". False otherwise.
%% It is considered "fixed" iff it's length is fixed at compile time.
%% The function is only defined for expressions that may appear in a pattern.
-spec is_fixed(expr()) -> boolean().
is_fixed(#expr{body = #literal{}}) -> true;
is_fixed(#expr{body = #array{}})   -> true;
is_fixed(#expr{body = #var{action=A}}) ->
	A =:= access orelse A =:= lastaccess;
is_fixed(#expr{body = #strcat{fixed=Fixed}}) -> Fixed;
is_fixed(#expr{body = #arrcat{fixed=Fixed}}) -> Fixed.

%% @doc Annotates an abstract syntax tree.
-spec annotate(expr()) -> {ok, expr()}
                        | {error, {lsrtyper, Message::iolist()}}.
annotate(Tree) ->
	NestedScope = [],
	try t_module(Tree, NestedScope) of
		{AnnotatedTree, _ModifiedScope} -> {ok, AnnotatedTree}
	catch
		throw:Message -> {error, {lsrtyper, Message}}
	end.

t_module(Module = #module{defs = Defs}, Scope) ->
	%% Add all names to the scope with the type 'fun'.
	%% TODO: type-check, at least add arity, e.g. {'fun', [any, any], any}
	ModuleScope = orddict:map(fun(_Name, _Funs) -> 'fun' end, Defs),
	Scope1 = [ModuleScope | Scope],
	DictMapFun = fun(_Name, Funs) -> t_funs(Funs, Scope1) end,
	{Module#module{defs = orddict:map(DictMapFun, Defs),
	               types = ModuleScope},
	 Scope}.

t_funs(nil, Scope) -> {nil, Scope};
t_funs(Cons = #funcons{head = Fun, tail = Funs}, Scope) ->
	{Fun1 = #'fun'{accessed = A1}, Scope1} = t_fun(Fun, Scope),
	{Funs1, Scope2} = t_funs(Funs, Scope1),
	A2 = case Funs1 of
		nil -> ordsets:new();
		#funcons{accessed = FunconsAccessed} -> FunconsAccessed
	end,
	{Cons#funcons{head = Fun1, tail = Funs1,
	              accessed = ordsets:union(A1, A2)},
	 Scope2}.

t_fun(Fun = #'fun'{params = Params, body = Body}, Scope) ->
	{Params1, Scope1} = t_patterns(Params, [[] | Scope]),
	{Body1 = #expr{type = _Type, accessed = AccessedInBody},
	 [LocalScope|Scope2]} = t(Body, Scope1),

	%% Mark last accesses
	LocalVarSet = scope_to_varset(LocalScope),
	LocalAccessedInBody = ordsets:intersection(LocalVarSet, AccessedInBody),
	Body2 = mark_last_accesses(Body1, LocalAccessedInBody),
	AccessedInParams = exprs_get_accessed(Params1),
	AccessedInParamsOnly = ordsets:subtract(LocalVarSet, AccessedInBody),
	Params2 = foldl_swapped(fun exprs_mark_last_access/2,
	                        Params1,
	                        AccessedInParamsOnly),

	%% Accessed vars in outer scope only
	Accessed = ordsets:subtract(ordsets:union(AccessedInParams,
	                                          AccessedInBody),
	                            LocalVarSet),

	%% TODO: Set type to e.g. {'fun', paramtypes, returntype}
	Fun1 = Fun#'fun'{params = Params2, body = Body2, locals = LocalScope,
	                 accessed = Accessed, type = 'fun'},
	{Fun1, Scope2}.

%% @doc Checks and type-matches a pattern against a type
-spec t_pattern(expr(), typename(), nested_scope()) -> {expr(), nested_scope()}.
t_pattern(Expr = #expr{body = Var = #var{name = "_"}}, T, Scope) ->
	Expr2 = Expr#expr{body = Var#var{action = discard},
	                  type = T,
	                  accessed = ordsets:new()},
	{Expr2, Scope};
t_pattern(Expr = #expr{body = Var = #var{name=Name}, line = Line}, T, Scope) ->
	case lookup_var(Name, Scope) of
		undefined ->
			%% New variable. Bind and add it to the scope with type T.
			Scope2 = add_var(Name, T, Scope),
			Expr2 = Expr#expr{body = Var#var{action = bind},
			                  type = T,
			                  accessed = ordsets:from_list([Name])},
			{Expr2, Scope2};
		VarType ->
			%% Catch obvious type errors
			is_subtype_of(T, VarType) orelse is_subtype_of(VarType, T) orelse
				throw(io_lib:format("Type mismatch for variable ~s on line ~p."
				                    " Got ~p, expected ~p.",
				                    [Name, Line, VarType, T])),
			Expr2 = Expr#expr{body = Var#var{action = access},
			                  type = T,
			                  accessed = ordsets:from_list([Name])},
			{Expr2, Scope}
	end;
t_pattern(Expr = #expr{body = #literal{type = Type}}, T, Scope) ->
	case is_subtype_of(Type, T) of
		true ->
			{Expr#expr{type = Type, accessed = ordsets:new()}, Scope};
		false ->
			#expr{body = #literal{data = Data}, line = Line} = Expr,
			throw(io_lib:format("Found ~p literal where ~p expected,"
			                    " on line ~p, near ~p.~n",
			                    [Type, T, Line, Data]))
	end;
t_pattern(Pat = #expr{body = Array = #array{elems = Elems},
                      line = Line},
          T, Scope) ->
	is_subtype_of(array, T) orelse
		throw(io_lib:format("Can't match ~p against array pattern "
		                    "on line ~p.",
		                    [T, Line])),
	{Elems1, Scope1} = t_patterns(Elems, Scope),
	Accessed = case Elems1 of
		nil                 -> ordsets:new();
		#cons{accessed = A} -> A
	end,
	Pat1 = Pat#expr{body = Array#array{elems = Elems1},
	                type = array,
	                accessed = Accessed},
	{Pat1, Scope1};

t_pattern(Pat = #expr{body = Strcat = #strcat{left = L, right = R},
                      line = Line},
          T, Scope) ->
	{L1 = #expr{type = T1, accessed = A1}, Scope1} = t_pattern(L, T, Scope),
	{R1 = #expr{type = T2, accessed = A2}, Scope2} = t_pattern(R, T, Scope1),
	is_subtype_of(string, T) orelse
		throw(io_lib:format("Can't match ~p against string concatenation "
		                    "on line ~p.",
		                    [T, Line])),
	is_subtype_of(string, T1) orelse
		throw(io_lib:format("Can't use ~p on the left side in string "
		                    "concatenation, in pattern on line ~p",
		                    [T1, Line])),
	is_subtype_of(string, T2) orelse
		throw(io_lib:format("Can't use ~p on the right side in string "
		                    "concatenation in pattern on line ~p",
		                    [T2, Line])),
	F1 = is_fixed(L1),
	F2 = is_fixed(R1),
	F1 or F2 orelse throw(io_lib:format("At least one operand of ~~ must be "
	                                    "fixed, on line ~p.",
		                                [Line])),
	Pat1 = Pat#expr{body = Strcat#strcat{left = L1,
	                                     right = R1,
	                                     fixed = F1 and F2},
	                type = string,
	                accessed = ordsets:union(A1, A2)},
	{Pat1, Scope2};

t_pattern(Pat = #expr{body = Arrcat = #arrcat{left = L, right = R},
                      line = Line},
          T, Scope) ->
	{L1 = #expr{type = T1, accessed = A1}, Scope1} = t_pattern(L, T, Scope),
	{R1 = #expr{type = T2, accessed = A2}, Scope2} = t_pattern(R, T, Scope1),
	is_subtype_of(array, T) orelse
		throw(io_lib:format("Can't match ~p against array concatenation "
		                    "on line ~p.",
		                    [T, Line])),
	is_subtype_of(array, T1) orelse
		throw(io_lib:format("Can't use ~p on the left side in array "
		                    "concatenation, in pattern on line ~p",
		                    [T1, Line])),
	is_subtype_of(array, T2) orelse
		throw(io_lib:format("Can't use ~p on the right side in array "
		                    "concatenation in pattern on line ~p",
		                    [T2, Line])),
	F1 = is_fixed(L1),
	F2 = is_fixed(R1),
	F1 or F2 orelse throw(io_lib:format("At least one operand of @ must be "
	                                    "fixed, on line ~p.",
		                                [Line])),
	Pat1 = Pat#expr{body = Arrcat#arrcat{left = L1,
	                                     right = R1,
	                                     fixed = F1 and F2},
	                type = array,
	                accessed = ordsets:union(A1, A2)},
	{Pat1, Scope2};

t_pattern(#expr{line = Line}, _T, _Scope) ->
	throw(io_lib:format("Invalid pattern on line ~p.", [Line])).

%% @doc Used for array patterns and function parameters (list of patterns)
-spec t_patterns(exprs(), nested_scope()) ->
	{exprs(), nested_scope()}.
t_patterns(nil, Scope) ->
	{nil, Scope};
t_patterns(Cons = #cons{head=Head, tail=Tail}, Scope) ->
	{Head1 = #expr{accessed=A1}, Scope1} = t_pattern(Head, any, Scope),
	{Tail1, Scope2} = t_patterns(Tail, Scope1),
	A2 = case Tail1 of
		nil               -> ordsets:new();
		#cons{accessed=A} -> A
	end,
	%% TODO: set type to sth. like [type-of-head | type-of-tail]
	Cons1 = Cons#cons{head = Head1,
	                  tail = Tail1,
	                  accessed = ordsets:union(A1, A2)},
	{Cons1, Scope2}.

%% The cases "rules" for the case ... of ... construct
-spec t_rules(#rulecons{}, SubjectType::typename(), nested_scope()) ->
	{#rulecons{}, ReturnType::typename(), nested_scope()}.
t_rules(RuleCons = #rulecons{head = Rule, tail = nil},
        T, Scope) ->
	{Rule1 = #rule{pat  = #expr{accessed = A1},
	               expr = #expr{type = RetType, accessed = A2}},
	 Scope1} = t_rule(Rule, T, Scope),
	RuleCons1 = RuleCons#rulecons{head = Rule1,
	                              accessed = ordsets:union(A1, A2)},
	{RuleCons1, RetType, Scope1};
t_rules(RuleCons = #rulecons{head = Rule, tail = Tail = #rulecons{}},
        T, Scope) ->
	{Rule1 = #rule{pat  = #expr{accessed = A1},
	               expr = #expr{type = RuleRetType, accessed = A2}},
	 Scope1} = t_rule(Rule, T, Scope),
	{Tail1 = #rulecons{accessed = A3}, TailRetType, Scope2} =
		t_rules(Tail, T, Scope1),
	RetType = get_common_supertype(RuleRetType, TailRetType),
	RuleCons1 = RuleCons#rulecons{head = Rule1,
	                              tail = Tail1,
	                              accessed = ordsets:union([A1, A2, A3])},
	{RuleCons1, RetType, Scope2}.

%% @ A rule on the form PATTERN -> CODE
-spec t_rule(rule(), SubjectType::typename(), nested_scope()) ->
	{rule(), ReturnType::typename(), nested_scope()}.
t_rule(Rule = #rule{pat = Pat, expr = Expr}, T, Scope) ->
	{Pat1, Scope1} = t_pattern(Pat, T, Scope),
	{Expr1, Scope2} = t(Expr, Scope1),
	Rule1 = Rule#rule{pat = Pat1, expr = Expr1},
	{Rule1, Scope2}.

%% @doc Checks and annotates an expression. Returns the annotated expression
%% and the possibly modified scope.
%% Throws an error message (iolist) on type errors.
-spec t(expr(), nested_scope()) -> {expr(), nested_scope()}.

%% Do block. A new scope.
t(Expr = #expr{body = #do{expr = E}}, Scope) ->
	InnerScope = [[] | Scope],
	{E1, [LocalVars | Scope1]} = t(E, InnerScope),
	LocalVarsSet = scope_to_varset(LocalVars),
	E2 = #expr{type = Type, accessed = A} = mark_last_accesses(E1, LocalVarsSet),
	A2 = ordsets:subtract(A, LocalVarsSet),
	Expr1 = Expr#expr{body = #do{expr = E2, locals = LocalVars},
	                  type = Type,
	                  accessed = A2},
	{Expr1, Scope1};

%% Sequence: Eval E1 and thow away value, then eval E2.
t(Expr = #expr{body = Binop = #binop{op=seq, left=E1, right=E2}}, Scope) ->
	{E1a = #expr{accessed = A1}, Scope1} = t(E1, Scope),
	{E2a = #expr{accessed = A2, type = T}, Scope2} = t(E2, Scope1),
	Expr1 = Expr#expr{body = Binop#binop{left=E1a, right=E2a},
	                  type = T,
	                  accessed = ordsets:union(A1, A2)},
	{Expr1, Scope2};

%% Variable access
t(#expr{body = #var{name = "_"}, line = Line}, _) ->
	throw(io_lib:format("The variable _ can not be used in this context, "
	                    "on line ~p",
	                    [Line]));
t(Expr = #expr{body = Var = #var{name = Name}, line = Line}, Scope) ->
	case lookup_var(Name, Scope) of
		undefined ->
			throw(io_lib:format("Undefined variable: ~s on line ~p",
			                    [Name, Line]));
		Type ->
			Expr1 = Expr#expr{body = Var#var{action = access},
			                  type = Type,
			                  accessed = ordsets:from_list([Name])},
			{Expr1, Scope}
	end;

%% Literal
t(Expr = #expr{body = #literal{type = Type}}, Scope) ->
	{Expr#expr{type = Type, accessed = ordsets:new()}, Scope};

%% if-then-else
t(Expr = #expr{body = If = #'if'{'cond' = E1, 'then' = E2, 'else' = E3},
               line = Line},
  Scope) ->
	{Ae1 = #expr{type = T1, accessed = A1}, Scope1} = t(E1, Scope),
	assert_type(boolean, T1, "Condition for if-then-else", Line),
	%% TODO Fix the local scopes of 'then' and 'else'
	{Ae2 = #expr{type = T2, accessed = A2}, Scope2} = t(E2, Scope1),
	{Ae3 = #expr{type = T3, accessed = A3}, Scope3} = t(E3, Scope2),
	Expr1 = Expr#expr{body = If#'if'{'cond' = Ae1, 'then' = Ae2, 'else' = Ae3},
	                  type = get_common_supertype(T2, T3),
	                  accessed = ordsets:union([A1, A2, A3])},
	{Expr1, Scope3};

t(#expr{body = #'case'{rules = nil}, line = Line}, _) ->
	throw(iolib:format("Case without cases can never succeed, on line ~p",
	                   [Line]));
t(Expr = #expr{body = Case = #'case'{test = Test, rules = Rules = #rulecons{}}},
  Scope) ->
  	{Test1 = #expr{type = SubjType, accessed = A1}, Scope1} = t(Test, Scope),
	{Rules1 = #rulecons{accessed = A2}, RetType, Scope2} =
		t_rules(Rules, SubjType, Scope1),
	Case1 = Case#'case'{test = Test1, rules = Rules1},
	Expr1 = Expr#expr{body = Case1, type = RetType, accessed = ordsets:union(A1, A2)},
	{Expr1, Scope2};

%% String concatenation
t(Expr = #expr{body = Strcat = #strcat{left = E1, right = E2}, line = Line},
  Scope) ->
	{Ae1 = #expr{type = T1, accessed = A1}, Scope1} = t(E1, Scope),
	{Ae2 = #expr{type = T2, accessed = A2}, Scope2} = t(E2, Scope1),
	is_subtype_of(string, T1) orelse
		throw(io_lib:format("Can't use ~p on the left side in string "
		                    "concatenation on line ~p",
		                    [T1, Line])),
	is_subtype_of(string, T2) orelse
		throw(io_lib:format("Can't use ~p on the right side in string "
		                    "concatenation on line ~p",
		                    [T2, Line])),
	Expr1 = Expr#expr{body = Strcat#strcat{left = Ae1,
	                                       right = Ae2,
	                                       fixed = true},
	                  type = string,
	                  accessed = ordsets:union(A1, A2)},
	{Expr1, Scope2};

%% Array concatenation
t(Expr = #expr{body = Arrcat = #arrcat{left = E1, right = E2}, line = Line},
  Scope) ->
	{Ae1 = #expr{type = T1, accessed = A1}, Scope1} = t(E1, Scope),
	{Ae2 = #expr{type = T2, accessed = A2}, Scope2} = t(E2, Scope1),
	is_subtype_of(array, T1) orelse
		throw(io_lib:format("Can't use ~p on the left side in array "
		                    "concatenation on line ~p",
		                    [T1, Line])),
	is_subtype_of(array, T2) orelse
		throw(io_lib:format("Can't use ~p on the right side in array "
		                    "concatenation on line ~p",
		                    [T2, Line])),
	Expr1 = Expr#expr{body = Arrcat#arrcat{left = Ae1,
	                                       right = Ae2,
	                                       fixed = true},
	                  type = array,
	                  accessed = ordsets:union(A1, A2)},
	{Expr1, Scope2};

t(Expr = #expr{body = Assign = #assign{pat=P, expr=E}}, Scope) ->
	{E1 = #expr{type = T, accessed = A1}, Scope1} = t(E, Scope),
	{P1 = #expr{type = T, accessed = A2}, Scope2} = t_pattern(P, T, Scope1),
	Expr1 = Expr#expr{body = Assign#assign{pat=P1, expr=E1},
	                  type = T,
	                  accessed = ordsets:union(A1, A2)},
	{Expr1, Scope2};

%% For arrays
%% TODO Add type to each element and infer it to array of <type>
t(Expr = #expr{body = Array = #array{elems = Es}}, Scope) ->
	{Es1, Scope1} = t_exprs(Es, Scope),
	A = exprs_get_accessed(Es1),
	Expr1 = Expr#expr{body = Array#array{elems = Es1},
	                  type = array,
	                  accessed = A},
	{Expr1, Scope1}.

-spec t_exprs(exprs(), scope()) -> {exprs(), scope()}.
t_exprs(nil, Scope) ->
	{nil, Scope};
t_exprs(Exprs = #cons{head = Head, tail = Tail}, Scope) ->
	{Head1 = #expr{accessed = A1}, Scope1} = t(Head, Scope),
	{Tail1, Scope2} = t_exprs(Tail, Scope1),
	A2 = case Tail1 of
		nil                  -> ordsets:new();
		#cons{accessed = A}  -> A
	end,
	Exprs1 = Exprs#cons{head = Head1,
	                    tail = Tail1,
	                    accessed = ordsets:union(A1, A2)},
	{Exprs1, Scope2}.

%% Lookup a variable in a nested scope. Returns its type or undefined.
lookup_var(_Name, []) ->
	undefined;
lookup_var(Name, [S|Ss]) ->
	case proplists:get_value(Name, S) of
		undefined -> lookup_var(Name, Ss);
		Type	      -> Type
	end.

%% Add a variable to a nested scope. Doesn't check if it already exists.
add_var(Name, Type, [S|Ss]) ->
	[[{Name, Type} | S] | Ss].

%------------------------------------------------------------------------------

%% @doc Foldl with the function taking its two args swapped
-spec foldl_swapped(Fun::fun((A, B) -> A), Acc0::A, List::[B]) -> Acc1::A.
foldl_swapped(Fun, Acc0, List) ->
	lists:foldl(fun (Elem, AccIn) -> Fun(AccIn, Elem) end, Acc0, List).

% Functions to annotate the last access for a list of variables

%% @doc Marks the last access of every variable in the expression subtree.
%% Returns the annotated expression with the last accesses marked.
-spec mark_last_accesses(#expr{}, varset()) -> #expr{}.
mark_last_accesses(Expr, Vars) ->
	foldl_swapped(fun mark_last_access/2, Expr, Vars).

%	Expr;
%mark_last_accesses(Expr, [Name|Vars]) ->
%	Expr1 = mark_last_access(Expr, Name),
%	mark_last_accesses(Expr1, Vars).

-spec exprs_mark_last_access(exprs(), string()) -> exprs().
exprs_mark_last_access(Cons = #cons{head = Head, tail = Tail, accessed = A},
                         Name) ->
	%% Asserting that we're not trying to mark in a bad branch
	true = ordsets:is_element(Name, A),
	case ordsets:is_element(Name, exprs_get_accessed(Tail)) of
		true  -> Cons#cons{tail = exprs_mark_last_access(Tail, Name)};
		false -> Cons#cons{head = mark_last_access(Head, Name)}
	end.

-spec rules_mark_last_access(rules(), string()) -> rules().
rules_mark_last_access(Rs = #rulecons{head = Rule, tail = Tail, accessed = A},
                       Name) ->
	%% Asserting that we're not trying to mark in a bad branch
	true = ordsets:is_element(Name, A),
	Rule1 = rule_mark_last_access(Rule, Name),
	Tail1 = case ordsets:is_element(Name, rules_get_accessed(Tail)) of
		true  -> rules_mark_last_access(Tail, Name);
		false -> Tail
	end,
	Rs#rulecons{head = Rule1, tail = Tail1}.

-spec rule_mark_last_access(rule(), string()) -> rules().
rule_mark_last_access(R = #rule{pat = Pat, expr = Expr}, Name) ->
	InExpr = ordsets:is_element(Name, get_accessed(Expr)),
	InPat  = ordsets:is_element(Name, get_accessed(Pat)),
	if
		InExpr -> R#rule{expr = mark_last_access(Expr, Name)};
		InPat  -> R#rule{pat  = mark_last_access(Expr, Name)};
		true   -> R
	end.

%% @doc Marks the last access of a variable in the expression subtree.
%% Returns the annotated expression with the last accesses marked.
%% Throws a message (iolist) if an unused variable is detected.
-spec mark_last_access(expr(), string()) -> expr().
mark_last_access(Expr = #expr{body = Body, accessed = A}, Name) ->
	%% Asserting that this function is used on an Expr where Name is accessed
	true = ordsets:is_element(Name, A),
	Body1 = case Body of
		#var{name = Name, action = access} ->
			Body#var{action = lastaccess};
		#var{name = [$_|_], action = bind} ->
			%% The variable is never used, but don't warn when the name starts
			%% with an underscore.
			Body#var{action = discard};
		#var{name = Name, action = bind} ->
			#expr{line = Line} = Expr,
			throw(io_lib:format("Variable ~p on line ~p is never used.",
			                    [Name, Line]));
		#array{elems = Es} ->
			%% Accessed ok. In which of them for the last time?
			Es1 = exprs_mark_last_access(Es, Name),
			Body#array{elems = Es1};
		#strcat{left = E1, right = E2} ->
			%% Accessed ok. In left, right or both?
			case ordsets:is_element(Name, get_accessed(E2)) of
				true  -> Body#strcat{right = mark_last_access(E2, Name)};
				false -> Body#strcat{left = mark_last_access(E1, Name)}
			end;
		#arrcat{left = E1, right = E2} ->
			%% Accessed ok. In left, right or both?
			case ordsets:is_element(Name, get_accessed(E2)) of
				true  -> Body#arrcat{right = mark_last_access(E2, Name)};
				false -> Body#arrcat{left = mark_last_access(E1, Name)}
			end;
		#binop{op=Op, left=E1, right=E2} when Op =:= seq ->
			%% Accessed ok. In left, right or both?
			case ordsets:is_element(Name, get_accessed(E2)) of
				true  -> Body#binop{right = mark_last_access(E2, Name)};
				false -> Body#binop{left = mark_last_access(E1, Name)}
			end;
		#assign{pat=P, expr=E} ->
			%% Accessed ok. In expr, pattern or both?
			case ordsets:is_element(Name, get_accessed(P)) of
				true  -> Body#assign{pat = mark_last_access(P, Name)};
				false -> Body#assign{expr = mark_last_access(E, Name)}
			end;
		#do{expr = E} ->
			Body#do{expr = mark_last_access(E, Name)};
		#'if'{'cond'=E1, 'then'=E2, 'else'=E3} ->
			%% Accessed ok. In 'then', in 'else' or only in the condition?
			InThen = ordsets:is_element(Name, get_accessed(E2)),
			InElse = ordsets:is_element(Name, get_accessed(E3)),
			if
				InThen andalso InElse ->
					Body#'if'{'then' = mark_last_access(E2, Name),
					          'else' = mark_last_access(E3, Name)};
				InThen ->
					Body#'if'{'then' = mark_last_access(E2, Name)};
				InElse ->
					Body#'if'{'else' = mark_last_access(E3, Name)};
				true ->
					Body#'if'{'cond' = mark_last_access(E1, Name)}
			end;
		#'case'{test = E, rules = Rs = #rulecons{accessed = A}} ->
			case ordsets:is_element(Name, A) of
				true  -> Body#'case'{rules = rules_mark_last_access(Rs, Name)};
				false -> Body#'case'{test = mark_last_access(E, Name)}
			end
	end,
	Expr#expr{body = Body1}.
