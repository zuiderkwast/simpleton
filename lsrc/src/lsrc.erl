-module(lsrc).

-export([compile/1, compile/2]).

-record(state, {nexttmp=1, indent=0}).

compile(Src) ->
	compile(Src, false).

%% @doc Compiles Lesser souce code to C code.  Prints an error message on
%% failure.  TODO: Should return {ok, Code} or {error, Message}...
compile(Bin, Verbose) when is_binary(Bin) ->
	compile(binary_to_list(Bin), Verbose);
compile(String, Verbose) when is_list(String) ->
	try
		{ok, Tokens, _Line} = lsrlexer:string(String),
		% io:format("Tokens: ~p~n", [Tokens]),
		{ok, Tree} = lsrparser:parse(Tokens),
		case Verbose of
			true  -> io:format("Tree:~n~p~n", [Tree]);
			false -> ok
		end,
		{ok, Annotated} = lsrtyper:annotate(Tree),
		case Verbose of
			true  -> io:format("Annotated tree:~n~p~n", [Annotated]);
			false -> ok
		end,
		c_prog(Annotated)
	catch
		error:{badmatch, {error, Error}} ->
			Message1 = case Error of
				{Line, lsrparser, Message} ->
					["Syntax: ", Message, " on line ", integer_to_list(Line)];
				{lsrtyper, Message} ->
					["Type: ", Message];
				Other ->
					["Other: ", io_lib:format("~p", [Other])]
			end,
			io:format("~s~n", [Message1])
	end.

c_prog({prog, E, Type, Vars, _OuterVars=[]}) ->
	State = #state{},
	State1 = indent_more(State),
	#state{indent = Indent} = State1,
	{Decl, Code, RetVar, _State2} = c(E, State1),
	MemDebug = true,
	Output =
	[case MemDebug of
		true -> "#define LSR_MONITOR_ALLOC\n";
		false -> []
	 end,
	 "#include \"runtime/runtime.h\"\n",
	 "int main() {\n",
	 "\n",
	 indent(Indent), "/* local vars */\n",
	 c_vardecls(Vars, Indent),
	 "\n",
	 indent(Indent), "/* temporary vars */\n",
	 Decl,
	 "\n",
	 indent(Indent), "/* code */\n",
	 Code,
	 "\n",
	 indent(Indent), "/* print the return value */\n",
	 %% Assert that the return value is a string
	 case Type of
		string -> [];
		_      -> [indent(Indent), "lsr_assert_string(", RetVar, ");\n"]
	 end,	
	 indent(Indent), "printf(\"%s\\n\", lsr_chars(", RetVar, "));\n",
	 indent(Indent), "lsr_free_unused(", RetVar, ");\n",
	 case MemDebug of
		true ->
			[indent(Indent), 
			 "printf(\"Mem leakage: %d\\nTotal num allocs: %d\\n\", "
			 "lsr_alloc_bytes_cnt, lsr_num_allocs);\n"];
		false -> []
	 end,
	 indent(Indent), "return 0;\n",
	 "}\n"],
	list_to_binary(Output).

c_vardecls(Vars, Indent) ->
	c_vardecls(Vars, Indent, []).
c_vardecls([], _, Acc) ->
	lists:reverse(Acc);
c_vardecls([{Name, Type} | Vars], Indent, Acc) ->
	c_vardecls(Vars, Indent, [decl(Name, Type, Indent) | Acc]).

%% @doc Check a value (c variable) against a pattern. The SuccessBool is a
%% c bool expression indicating success.
-spec c_match(Pattern::lsrtyper:aexpr(), Value::string(),
              ValueType::lsrtyper:typename(), #state{}) ->
      {MatchDecl::iolist(), MatchCode::iolist(), SuccessBool::iolist(),
       BindDecl::iolist(), BindCode::iolist(), #state{}}.
c_match({bind, _, VarName, VarType}, Value, ValueType,
        State = #state{indent=Indent}) ->
	% Assert subtype
	true = lsrtyper:is_subtype_of(ValueType, VarType),
	% Assignment code, increment ref-counter
	BindCode = [indent(Indent), VarName, " = ", Value, ";",
	            case VarType of
					boolean -> [];
					_       -> [" lsr_incref(", VarName, ");"]
				end,
	            "\n"],
	{[], [], "true",
	 [], BindCode, State};

c_match(Var = {var, _, _, VarType, _}, Value, ValueType, State) ->
	% Assert subtype (should be infered. TODO: move to typechecker)
	true = lsrtyper:is_subtype_of(ValueType, VarType),
	% Compile the var access
	{Decl, Code, Name, State2} = c(Var, State),
	SuccessBool = ["lsr_equals(", Name, ", ", Value, ")"],
	{Decl, Code, SuccessBool, [], [], State2};

c_match(Lit = {literal, _}, Value, ValueType, State) ->
	% Assert supertype (should be infered. TODO: move to typechecker)
	true = lsrtyper:is_subtype_of(lsrtyper:get_type(Lit), ValueType),
	% Compile the literal
	{Decl, Code, Name, State2} = c(Lit, State),
	SuccessBool = ["lsr_equals(", Name, ", ", Value, ")"],
	{Decl, Code, SuccessBool, [], [], State2}.

c({'if', E1, E2, E3, T, _As}, State = #state{indent=Indent}) ->
	% TODO Free the input values (i.e. the condition).
	% TODO Free all vars in the dead branch which would be last accessed there.
	A2 = lsrtyper:aexpr_get_accesses(E2),
	A3 = lsrtyper:aexpr_get_accesses(E3),
	AccessedInElseOnly = lsrvarsets:subtract(A3, A2),
	AccessedInThenOnly = lsrvarsets:subtract(A2, A3),
	Discard2 = discard_vars(AccessedInElseOnly, Indent + 1),
	Discard3 = discard_vars(AccessedInThenOnly, Indent + 1),
	% TODO Push new local scope to state.
	{Decl1, Code1, RetVar1, State1} = c(E1, State),
	BoolCheck = case lsrtyper:get_type(E1) of
		boolean ->
			% We know it's boolean.  No runtime check.
			[];
		_ ->
			% Code to ensure it's a boolean.
			[indent(Indent), "lsr_ensure_boolean(", RetVar1, ");\n"]
	end,
	{RetVar, State2} = new_tmpvar(State1),
	{Decl2, Code2, RetVar2, State3} = c(E2, indent_more(State2)),
	{Decl3, Code3, RetVar3, State4} = c(E3, State3),
	State5 = indent_less(State4),
	Decl = [Decl1, decl(RetVar, T, Indent)],
	Code = [Code1, BoolCheck,
	        indent(Indent), "if (lsr_ptr_to_bool(", RetVar1, ")) {\n",
	        Decl2, Discard2, Code2,
	        indent(Indent + 1), RetVar, " = ", RetVar2, ";\n",
	        indent(Indent), "} else {\n", 
	        Decl3, Discard3, Code3,
	        indent(Indent + 1), RetVar, " = ", RetVar3, ";\n",
	        indent(Indent), "}\n"
	       ],
	{Decl, Code, RetVar, State5};

% Sequence: Discard the first value and continue. Keep only the last value. 
c({seq, E1, E2, _Type, _Accesses}, State = #state{indent=Indent}) ->
	{Decl1, Code1, DeadVar, State2} = c(E1, State),
	{Decl2, Code2, RetVar, State3} = c(E2, State2),
	FreeCode = case E1 of
		{assign, {bind, _, _, _}, _, _, _} ->
			% A variable has just ben assign this value.  It can't be the last
			% access.
			[];
		_ ->
			Accesses = lsrtyper:aexpr_get_accesses(E2),
			case lsrvarsets:is_element(DeadVar, Accesses) of
				false -> [indent(Indent), "lsr_free_unused(", DeadVar, ");\n"];
				true  -> [] % DeadVar will be used later, so refc can't be zero here.
			end
	end,
	{[Decl1, Decl2], [Code1, FreeCode, Code2], RetVar, State3};

% Concatenation
c({concat, Left, Right, _Type, _Accesses}, State = #state{indent=Indent}) ->
	{LeftDecl, LeftCode, LeftVar, State2} = c(Left, State),
	%% If it's not infered to be a string, add a runtime assertion
	AssertLeft = case lsrtyper:get_type(Left) of
		string -> [];
		_      -> [indent(Indent), "lsr_assert_string(", LeftVar, ");\n"]
	end,	
	{RightDecl, RightCode, RightVar, State3} = c(Right, State2),
	AssertRight = case lsrtyper:get_type(Right) of
		string -> [];
		_      -> [indent(Indent), "lsr_assert_string(", RightVar, ");\n"]
	end,
	{RetVar, State4} = new_tmpvar(State3),
	ConcatCode = [indent(Indent), RetVar, " = lsr_string_concat(", LeftVar, ", ", RightVar, ");\n"],
	{[LeftDecl, RightDecl, decl(RetVar, string, Indent)],
	 [LeftCode, AssertLeft, RightCode, AssertRight, ConcatCode],
	 RetVar,
	 State4};

% Assignment (match) Pattern = Expression, binds any free vars in pattern
c({assign, Pattern, Expr, ExprType, _Accesses}, 
  State = #state{indent=Indent}) ->
	{ExprDecl, ExprCode, ExprVar, State2} = c(Expr, State),
	{MatchDecl, MatchCode, SuccessVar,
	 BindDecl, BindCode, State3} = c_match(Pattern, ExprVar, ExprType, State2),
	% Code to assert successed match and raise an error otherwise
	SuccessCode = case SuccessVar of
		"true" -> [];
		_      -> [indent(Indent),
	               "if (!", SuccessVar, ") lsr_error(\"Pattern mismatch (", ExprVar, ")\");\n"]
	end,
	{[ExprDecl, MatchDecl, BindDecl],
	 [ExprCode, MatchCode, SuccessCode, BindCode],
     ExprVar,
     State3};

% Variable access
c({var, _, Name, boolean, AccessType}, State = #state{indent=Indent}) ->
	% reference counted
	Code = case AccessType of
		lastaccess -> [indent(Indent),
		               "/* last access of boolean '", Name, "' */\n"];
		access     -> []
	end,
	{[], Code, Name, State};
c({var, _, Name, Type, AccessType}, State = #state{indent=Indent})
			when Type =:= string; Type =:= any ->
	Code = case AccessType of
		lastaccess -> [indent(Indent), "lsr_decref(", Name, ");",
		               " /* last access */\n"];
		access     -> []
	end,
	{[], Code, Name, State};

% Literals, either a plain C value or an initialized tempvar
c({literal, {boolean, _, Content}}, State = #state{indent=Indent}) ->
	% create boxed type and a new tmpvar
	{Name, State2} = new_tmpvar(State),
	Decl = [indent(Indent), c_type(boolean), " ", Name,
	        " = ", c_boolean_literal(Content), ";\n"],
	{Decl, [], Name, State2};
	%--------
	% We may want to use unboxed values, represented directly as a c bool
	%Ret = c_boolean_literal(Content),
	%{[], [], Ret, State};
c({literal, {string, _, Content}}, State = #state{indent=Indent}) ->
	% create boxed type and a new tmpvar
	{Name, State2} = new_tmpvar(State),
	Decl = [indent(Indent),
	        c_type(string), " ", Name, " = ",
	        c_string_literal(Content), ";\n"],
	{Decl, [], Name, State2}.

discard_vars(Names, Indent) ->
	lists:map(fun (Name) -> [indent(Indent), "lsr_discard_ref(", Name, "); /* never used after this point */\n"] end,
	          Names).

% Returns an unused temporary variable name (for intermediate values)
-spec new_tmpvar(#state{}) -> {string(), #state{}}.
new_tmpvar(State = #state{nexttmp=NextTmp}) ->
	{"tmp" ++ integer_to_list(NextTmp), State#state{nexttmp = NextTmp + 1}}.

c_type(_Type) ->
	%case Type of string -> "lsr_tagged_t *"; boolean -> "bool" end.
	"lsr_tagged_t *".

c_boolean_literal(Content) ->
	C_bool = case Content of true -> "true"; false -> "false" end,
	["lsr_bool_to_ptr(", C_bool, ")"].
c_string_literal(Content) ->
	["lsr_string_literal(", Content, ")"].

% declare and initialize
-spec decl(string(), lsrtyper:typename(), integer()) -> iolist().
decl(Name, Type, Indent) ->
	[indent(Indent), c_type(Type), " ", Name, ";",
	 " /* ", atom_to_list(Type), " */\n"].

indent(N) when N >= 0 ->
	string:chars($\s, N * 2).

indent_more(State = #state{indent = N}) ->
	State#state{indent = N + 1}.

indent_less(State = #state{indent = N}) ->
	State#state{indent = N - 1}.
