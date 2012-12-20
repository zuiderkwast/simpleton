%% @doc Typechecker/inferer/annotator for the syntax tree produced by the parser
-module(lsrtyper).

-export([annotate/1, aexpr_get_accesses/1, get_type/1, is_subtype_of/2]).

-type expr() :: lsrparser:expr().
-type prog() :: lsrparser:prog().
-type literal() :: lsrparser:literal().
-type arraycons() :: lsrparser:arraycons().

%% An atom representing a binary operator
-type binop() :: lsrparser:binop().

%% Types
-type typename() :: integer | float | number | boolean | string | array | any.

%% Variable scope
-type scope()        :: [{string(), typename()}].
-type nested_scope() :: [scope()].

%% Annotated AST
-type accesstype() :: access | lastaccess.

%% @doc Annotated variable
-type avar() :: {var, integer(), string(), typename(), accesstype()}.

%% @doc Annotated expression
-type aexpr() :: avar()
              | literal()
              | {binop(), aexpr(), aexpr(),
                 typename(), lsrvarsets:varset()}
              | {assign, aexpr(), aexpr(), typename(), lsrvarsets:varset()}
              | {'if', aexpr(), aexpr(), aexpr(), typename(), lsrvarsets:varset()}
              | {array, Length::integer(), aarraycons(), lsrvarsets:varset()}.

-type aarraycons() :: {arraycons, aexpr(), aarraycons(), lsrvarsets:varset()} | nil.

%% @doc Annotated program tree
-type aprog()  :: {prog, Body::aexpr(), ReturnType::typename(),
                  Locals::scope(), AccessesToOuterVars::lsrvarsets:varset()}.

%% @doc Returns the type of an annotated expression.
-spec get_type(aexpr()) -> typename().
get_type(Ae) ->
	case Ae of
		{var, _, _, T, _}     -> T;
		{literal, {T, _, _}}  -> T;
		{array, _, _, _}      -> array;
		{assign, _, _, T, _}  -> T;
		{'if', _, _, _, T, _} -> T;
		{_Binop, _, _, T, _}  -> T
	end.

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

-spec get_type_generality(typename()) -> integer().
get_type_generality(any)    -> 3;
get_type_generality(number) -> 2;
get_type_generality(_)      -> 1.

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

-spec scope_to_varset(scope()) -> lsrvarsets:lsrvarset().
scope_to_varset(Vars) ->
	scope_to_varset(Vars, []).
scope_to_varset([], Acc) ->
	lsrvarsets:from_list(Acc);
scope_to_varset([{Name, _Type}|Vars], Acc) ->
	scope_to_varset(Vars, [Name | Acc]).

%% @doc Annotates an abstract syntax tree.
-spec annotate(prog()) -> {ok, aprog()}
                        | {error, {lsrtyper, Message::iolist()}}.
annotate(Tree) ->
	NestedScope = [],
	try check_prog(Tree, NestedScope) of
		{AnnotatedTree, _ModifiedScope} -> {ok, AnnotatedTree}
	catch
		throw:Message -> {error, {lsrtyper, Message}}
	end.

% Prog (root symbol). A new scope.
-spec check_prog(prog(), nested_scope()) -> {aprog(), nested_scope()}.
check_prog({prog, E}, Scope) ->
	InnerScope = [[] | Scope],
	{Ae, [LocalVars | Scope3]} = t(E, InnerScope),
	Ae2 = mark_last_accesses(Ae, LocalVars),
	Accesses = aexpr_get_accesses(Ae2),
	Accesses2 = lsrvarsets:subtract(Accesses, scope_to_varset(LocalVars)),
	Type = get_type(Ae2),
	AnnotatedProg = {prog, Ae2, Type, LocalVars, Accesses2},
	{AnnotatedProg, Scope3}.

%% @doc Checks a pattern
-spec t_pattern(expr(), typename(), nested_scope()) -> {aexpr(), nested_scope()}.
t_pattern({var, Line, Name}, T, Scope) ->
	case lookup_var(Name, Scope) of
		undefined ->
			% New variable. Bind and add it to the scope with type T.
			Scope2 = add_var(Name, T, Scope),
			Avar = {bind, Line, Name, T},
			{Avar, Scope2};
		T ->
			% TODO Extend the rules for compatible subtypes
			Avar = {var, Line, Name, T, access},
			{Avar, Scope};
		BadT ->
			% TODO line and context of the error
			throw(io_lib:format("Type mismatch for variable ~s on line ~p."
							   " Got ~p, expected ~p.",
							   [Name, Line, BadT, T]))
	end;
t_pattern(Lit = {literal, {Type, Line, Data}}, T, Scope) ->
	case is_subtype_of(Type, T) of
		true ->
			{Lit, Scope};
		false ->
			throw(io_lib:format("Found ~p literal where ~p expected,"
			                   " on line ~p, near ~p.~n",
			                   [Type, T, Line, Data]))
	end;
t_pattern(Any, _T, _Scope) ->
	throw(io_lib:format("Invalid pattern ~p", [Any])).


assert_type(Expected, Found, What) ->
	case Found of
		Expected ->
			ok;
		_ ->
			throw(io_lib:format("Type mismatch: ~s. Found ~p, expected ~p.",
			                    [What, Found, Expected]))
	end.

%% @doc Checks and annotates an expression. Returns it and the possibly
%% modified scope.
%% Throws an error message (iolist) on type errors.
-spec t(expr(), nested_scope()) -> {aexpr(), nested_scope()}.

% Sequence: Eval E1 and thow away value, then eval E2.
t({seq, E1, E2}, Scope) ->
	{Ae1, Scope1} = t(E1, Scope),
	{Ae2, Scope2} = t(E2, Scope1),
	T  = get_type(Ae2),
	A1 = aexpr_get_accesses(Ae1),
	A2 = aexpr_get_accesses(Ae2),
	A = lsrvarsets:union(A1, A2),
	Ae = {seq, Ae1, Ae2, T, A},
	{Ae, Scope2};

% Variable access
t({var, Line, Name}, Scope) ->
	case lookup_var(Name, Scope) of
		undefined ->
			throw(io_lib:format("Undefined variable: ~s on line ~p",
			                    [Name, Line]));
		Type ->
			{{var, Line, Name, Type, access}, Scope}
	end;

% Literal
t(E = {literal, _}, Scope) ->
	{E, Scope};

% if-then-else
t({'if', E1, E2, E3}, Scope) ->
	{Ae1, Scope1} = t(E1, Scope),
	assert_type(boolean, get_type(Ae1), "Condition for if-then-else"),
	%% TODO Fix the local scopes of 'then' and 'else'
	{Ae2, Scope2} = t(E2, Scope1),
	{Ae3, Scope3} = t(E3, Scope2),
	T2 = get_type(Ae2),
	T3 = get_type(Ae3),
	T = get_common_supertype(T2, T3),
	%assert_type(T, T1, "The 'else' must match the 'then' clause"),
	A1 = aexpr_get_accesses(Ae1),
	A2 = aexpr_get_accesses(Ae2),
	A3 = aexpr_get_accesses(Ae3),
	A = lsrvarsets:union([A1, A2, A3]),
	{{'if', Ae1, Ae2, Ae3, T, A}, Scope3};

% Concatenation
t({concat, E1, E2}, Scope) ->
	{Ae1, Scope1} = t(E1, Scope),
	{Ae2, Scope2} = t(E2, Scope1),
	T1  = get_type(Ae1),
	T2  = get_type(Ae2),
	%% TODO Better message when mismatch, e.g. include line number.
	T = get_common_supertype(T1, T2),
	is_subtype_of(string, T) orelse is_subtype_of(array, T)
		orelse throw(["Can't concatenate ", T1, " with ", T2]),
	A1 = aexpr_get_accesses(Ae1),
	A2 = aexpr_get_accesses(Ae2),
	A = lsrvarsets:union(A1, A2),
	Ae = {concat, Ae1, Ae2, T, A},
	{Ae, Scope2};

t({assign, Pattern, Expr}, Scope) ->
	{Aexpr, Scope1} = t(Expr, Scope),
	T = get_type(Aexpr),
	A1 = aexpr_get_accesses(Aexpr),
	{Apattern, Scope2} = t_pattern(Pattern, T, Scope1),
	A2 = aexpr_get_accesses(Apattern),
	A = lsrvarsets:union(A1, A2),
	Ae = {assign, Apattern, Aexpr, T, A},
	{Ae, Scope2};

%% For arrays, aexpr :: {array, Len::integer(), aexprs(), lsrvarsets:varset()}
%% TODO Add type to each element and infer it to array of <type>
t({array, Len, Contents}, Scope) ->
	{AContents, Scope1} = t_arraycons(Contents, Scope),
	A = arraycons_get_accesses(AContents),
	{{array, Len, AContents, A}, Scope1}.

-spec t_arraycons(arraycons(), scope()) -> {aarraycons(), scope()}.
t_arraycons(nil, Scope) ->
	{nil, Scope};
t_arraycons({arraycons, E, Tail}, Scope) ->
	{Ae, Scope1} = t(E, Scope),
	{ATail, Scope2} = t_arraycons(Tail, Scope1),
	TailAccesses = case ATail of
		nil                  -> lsrvarsets:new();
		{arraycons, _, _, A} -> A
	end,
	HeadAccesses = aexpr_get_accesses(Ae),
	Accesses = lsrvarsets:union(HeadAccesses, TailAccesses),
	{{arraycons, Ae, ATail, Accesses}, Scope2}.

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

% Functions to annotate the last access for a list of variables, when possible

%% @doc Marks the last access of every variable in the expression subtree.
%% Returns the annotated expression with the last accesses marked.
-spec mark_last_accesses(aexpr(), scope()) -> aexpr().
mark_last_accesses(Ae, []) ->
	Ae;
mark_last_accesses(Ae, [{Name, _Type}|Vars]) ->
	Ae2 = mark_last_access(Ae, Name),
	mark_last_accesses(Ae2, Vars).

arraycons_get_accesses({arraycons, _, _, Accesses}) -> Accesses;
arraycons_get_accesses(nil) -> lsrvarsets:new().

-spec arraycons_mark_last_accesses(aarraycons(), string()) -> aarraycons().
arraycons_mark_last_accesses({arraycons, Ae, ATail, Accesses}, Name) ->
	%% Asserting that we're not trying to mark in a bad branch
	true = lsrvarsets:is_element(Name, Accesses),
	case lsrvarsets:is_element(Name, arraycons_get_accesses(ATail)) of
		true  ->
			ATail1 = arraycons_mark_last_accesses(ATail, Name),
			{arraycons, Ae, ATail1, Accesses};
		false ->
			Ae1 = mark_last_access(Ae, Name),
			{arraycons, Ae1, ATail, Accesses}
	end.

%% @doc Marks the last access of a variable in the expression subtree.
%% Returns the annotated expression with the last accesses marked.
-spec mark_last_access(aexpr(), string()) -> aexpr().
mark_last_access(Ae, Name) ->
	%% Asserting that this function is used on an Ae where Name is accessed
	true = lsrvarsets:is_element(Name, aexpr_get_accesses(Ae)),
	case Ae of
		{var, L, Name, T, _} ->
			{var, L, Name, T, lastaccess};
		{bind, Line, Name, _Type} ->
			throw(["Variable ", Name, " on line ", Line, " is never used."]);
		%{var, _, _, _, _} ->
		%	Ae;
		%{literal, _} ->
		%	Ae;
		{array, Len, Es, As} ->
			%% Accessed ok. In which of them for the last time?
			Es2 = arraycons_mark_last_accesses(Es, Name),
			{array, Len, Es2, As};
		{assign, P, E, T, As} ->
			%% Accessed ok. In expr, pattern or both?
			case lsrvarsets:is_element(Name, aexpr_get_accesses(P)) of
				true ->
					%% Accessed in P. Since P is evaluated after E, any
					%% last access must be in P.
					P2 = mark_last_access(P, Name),
					{assign, P2, E, T, As};
				false ->
					%% Not accessed in P, so any last is in E.
					E2 = mark_last_access(E, Name),
					{assign, P, E2, T, As}
			end;
		{'if', E1, E2, E3, T, As} ->
			%% Accessed ok. In 'then', in 'else' or only in the condition?
			InThen = lsrvarsets:is_element(Name, aexpr_get_accesses(E2)),
			InElse = lsrvarsets:is_element(Name, aexpr_get_accesses(E3)),
			{E1x, E2x, E3x} = if
				InThen andalso InElse ->
					{E1, mark_last_access(E2, Name),
					     mark_last_access(E3, Name)};
				InThen ->
					{E1, mark_last_access(E2, Name), E3};
				InElse ->
					{E1, E2, mark_last_access(E3, Name)};
				true ->
					{mark_last_access(E1, Name), E2, E3}
			end,
			{'if', E1x, E2x, E3x, T, As};
		{Binop, E1, E2, T, As} when Binop =:= concat; Binop =:= seq ->
			%% Accessed ok. In left, right or both?
			case lsrvarsets:is_element(Name, aexpr_get_accesses(E2)) of
				true ->
					%% Accessed in E2. Since E2 is evaluated after E1, any
					%% last access must be in E2.
					E2x = mark_last_access(E2, Name),
					{Binop, E1, E2x, T, As};
				false ->
					%% Not accessed in E2, so any last is in E1.
					E1x = mark_last_access(E1, Name),
					{Binop, E1x, E2, T, As}
			end
	end.

%------------------------------------------------------------------------------

% Functions for keeping track of accessed variables

%% @doc Returns a list of variables accessed or maybe accessed by the
%% (annotated) expression.
-spec aexpr_get_accesses(aexpr()) -> lsrvarsets:varset().
aexpr_get_accesses(Ae) ->
	case Ae of
		{var, _, Name, _, _}   -> lsrvarsets:from_list([Name]);
		{bind, _, Name, _}     -> lsrvarsets:from_list([Name]);
		{literal, _}           -> lsrvarsets:new();
		{array, _, _, As}      -> As;
		{'if', _, _, _, _, As} -> As;
		{assign, _, _, _, As}  -> As;
		{_Binop, _, _, _, As}  -> As
	end.
