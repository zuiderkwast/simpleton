-module(lsrc).

-export([compile/1, compile/2]).

-include("types.hrl").

-record(state, {nexttmp=1, nextlabel=1, indent=0}).

%% @doc A value that is subject to pattern matching
-record(subject, {value :: string(),       %% a C expression
                  type :: typename(),      %% infered type
                  offset = none,           %% offset, when used
                  length = none,           %% used together with offset
                  faillabel :: string()}). %% goto on mismatch

compile(Src) ->
	compile(Src, false).

%% @doc Compiles Lesser souce code to C code.
%% Returns {ok, Code} or {error, Message}.
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
		{ok, c_prog(Annotated, Verbose)}
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
			{error, Message1}
	end.

c_prog(#prog{body=E, type=Type, locals=Vars, accessed=_OuterVars=[]},
       MemDebug) ->
	State = #state{},
	State1 = indent_more(State),
	#state{indent = Indent} = State1,
	{Decl, Code, RetVar, _State2} = c(E, State1),
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

-spec c_vardecls(scope(), integer()) -> iolist().
c_vardecls(Vars, Indent) ->
	c_vardecls(Vars, Indent, []).
c_vardecls([], _, Acc) ->
	lists:reverse(Acc);
c_vardecls([{Name, Type} | Vars], Indent, Acc) ->
	c_vardecls(Vars, Indent, [decl(Name, Type, Indent) | Acc]).

%% @doc Check a value (c variable) against a pattern. The SuccessBool is a
%% c bool expression indicating success.
-spec c_match(Pattern::#expr{}, #subject{}, #state{}) ->
      {MatchDecl::iolist(), MatchCode::iolist(),
       BindDecl::iolist(), BindCode::iolist(), #state{}}.
c_match(#expr{body=#var{action=bind, name=VarName}, type=VarType},
        #subject{value=Value, offset=none, length=none},
        State = #state{indent=Indent}) ->
	% Assignment code, increment ref-counter
	BindCode = [indent(Indent), VarName, " = ", Value, ";",
	            case VarType of
	                boolean -> [];
	                _       -> [" lsr_incref(", VarName, ");"]
	            end,
	            "\n"],
	{[], [],	 [], BindCode, State};
c_match(#expr{body=#var{action=bind, name=VarName}},
        #subject{value=Value, type=string, offset=Offset, length=Length},
        State = #state{indent=Indent})
		when Offset =/= none, Length =/= none ->
	% Assignment code, increment ref-counter
	BindCode = [indent(Indent), VarName, " = "
	            "lsr_substr(", Value, ", ", Offset, ", ", Length, ");",
	            " lsr_incref(", VarName, ");\n"],
	{[], [],	 [], BindCode, State};


% Expression as pattern: compare equal to the pattern subject
c_match(Expr = #expr{body=Body}, Subject = #subject{}, State)
		when is_record(Body, var);
		     is_record(Body, literal) ->
	% Compile the expression
	{Decl, Code, ExprResult, State2} = c(Expr, State),
	EqCode = c_equality_check(ExprResult, Subject, State2),
	{Decl, [Code, EqCode], [], [], State2};

%% String concatenation as pattern. Assert string, find subject's length and
%% skip to helper.
c_match(Expr = #expr{body = #strcat{}, type = string},
        Subject = #subject{value=Value, type=ValueType, faillabel=FailLabel,
                           offset=none, length=none},
        State = #state{indent=Indent}) ->
	Offset = "0UL",
	TypeCheck = case ValueType of
		string -> [];
		_      -> [indent(Indent),
		           "if (!lsr_is_string(", Value, ")) goto ", FailLabel, ";\n"]
	end,
	{LenDecl, LenCode, LenVar, State1} = c_length(Value, string, State),
	Subject1 = Subject#subject{offset=Offset, length=LenVar, type=string},
	{Decl, Code, BindDecl, BindCode, State2} = c_match(Expr, Subject1, State1),
	{[LenDecl, Decl], [TypeCheck, LenCode, Code], BindDecl, BindCode, State2};

%% @doc Match a substring of Value. The type (string) is already checked.
%% Len and Offset are C expressions (such as variables)
c_match(#expr{body = #strcat{left = L, right = R}},
        Subject = #subject{faillabel=FailLabel,
                           offset=Offset, length=Len},
        State = #state{indent = Indent})
		when Offset =/= none, Len =/= none ->

	%% Decide sides
	{CheckExpr, RestPat, CheckSide} = case lsrtyper:is_fixed(L) of
		true  -> {L, R, left};
		false -> {R, L, right}
	end,

	%% Compile CheckExpr
	{Decl1, Code1, CheckVal, State1} = c(CheckExpr, State),
	{Decl2, Code2, CheckValLen, State2} = c_length(CheckVal, string, State1),

	%% Split Subject into CheckSubject and RestSubject
	{Decl3, Code3, CheckSubject, RestSubject, State3} =
		split_subject(Subject, CheckSide, CheckValLen, State2),

	%% A simple length assertion, since we have both lengths here
	#subject{length=CheckSubjLen} = CheckSubject,
	Code4 = case CheckValLen =:= CheckSubjLen of
		true ->
			[];
		false ->
			[indent(Indent), "if (", CheckValLen, " != ", CheckSubjLen, ") ",
			 "goto ", FailLabel, ";\n"]
	end,

	%% Check
	Code5 = c_equality_check(CheckVal, CheckSubject, State3),

	%% Match rest
	{Decl4, Code6, BindDecl, BindCode, State4} =
		c_match(RestPat, RestSubject, State3),
	{[Decl1, Decl2, Decl3, Decl4],
	 [Code1, Code2, Code3, Code4, Code5, Code6],
	 BindDecl, BindCode, State4}.

%% Split a pattern subject into a check part and a match part.
-spec split_subject(#subject{}, left | right, string(), #state{}) ->
	{Decl::iolist(), Code::iolist(), #subject{}, #subject{}, #state{}}.
split_subject(Subject = #subject{offset=Offset, length=Len},
              left, CheckLen,
              State = #state{indent=Indent}) ->
	{RestLen, State1} = new_tmpvar(State, "_rest_length"),
	Decl1 = [indent(Indent), "size_t ", RestLen, ";\n"],
	Code1 = [indent(Indent), RestLen, " = ", Len, " - ", CheckLen, ";\n"],
	{RestOffset, State2} = new_tmpvar(State1, "_rest_offset"),
	Decl2 = [indent(Indent), "size_t ", RestOffset, ";\n"],
	Code2 = [indent(Indent), RestOffset, " = ", Offset, " + ", CheckLen, ";\n"],
	{[Decl1, Decl2],
	 [Code1, Code2],
	 Subject#subject{length = CheckLen},
	 Subject#subject{offset = RestOffset, length = RestLen},
	 State2};
split_subject(Subject = #subject{offset=Offset, length=Len},
              right, CheckLen,
              State = #state{indent=Indent}) ->
	{RestLen, State1} = new_tmpvar(State, "_rest_length"),
	Decl1 = [indent(Indent), "size_t ", RestLen, ";\n"],
	Code1 = [indent(Indent), RestLen, " = ", Len, " - ", CheckLen, ";\n"],
	{CheckOffset, State2} = new_tmpvar(State1, "_check_offset"),
	Decl2 = [indent(Indent), "size_t ", CheckOffset, ";\n"],
	Code2 = [indent(Indent), CheckOffset, " = ", Offset, " + ", RestLen, ";\n"],
	{[Decl1, Decl2],
	 [Code1, Code2],
	 Subject#subject{offset = CheckOffset, length = CheckLen},
	 Subject#subject{length = RestLen},
	 State2}.

%% @doc Create C code for an equality check with goto faillabel on mismatch
-spec c_equality_check(CheckVar::string(), #subject{}, #state{}) -> iolist().
c_equality_check(CheckVar,
                 #subject{value=Value, offset=none, length=none,
                          faillabel=FailLabel},
                 #state{indent=Indent}) ->
	[indent(Indent), "if (!lsr_equals(", CheckVar, ", ", Value, ")) ",
	 "goto ", FailLabel, ";\n"];
c_equality_check(CheckVar,
                 #subject{value=Value, type=Type, offset=Offset, length=Length,
                          faillabel=FailLabel},
                 #state{indent=Indent}) ->
	EqFun = case Type of
		string -> "lsr_equals_substr";
		array  -> "lsr_equals_slice" %% TODO implement this in runtime
	end,
	[indent(Indent), "if (!", EqFun, "(", CheckVar, ", ", Value, ", ",
	 Offset, ", ", Length, ")) ", "goto ", FailLabel, ";\n"].


%% @doc C code for the length of a value (string or array)
-spec c_length(Value::iolist(), typename(), #state{}) ->
	{Decl::iolist(), Code::iolist(), RetVar::iolist(), State::#state{}}.
c_length(Value, Type, State = #state{indent = Indent}) ->
	LenFunc = case Type of
		string -> "lsr_strlen";
		array -> fixme
	end,
	{Var, State1} = new_tmpvar(State, "_length"),
	Decl = [indent(Indent), "size_t ", Var, ";\n"],
	Code = [indent(Indent), Var, " = ", LenFunc, "(", Value, ");\n"],
	{Decl, Code, Var, State1}.

-spec c(#expr{}, #state{}) -> {iolist(), iolist(), iolist(), #state{}}.
c(#expr{body = #'if'{'cond' = E1 = #expr{type = T1},
                     'then' = E2 = #expr{accessed = A2},
                     'else' = E3 = #expr{accessed = A3}},
        type = T},
  State = #state{indent=Indent}) ->
	% TODO Free the input values (i.e. the condition).
	% Free all vars in the dead branch which would be last accessed there.
	AccessedInElseOnly = lsrvarsets:subtract(A3, A2),
	AccessedInThenOnly = lsrvarsets:subtract(A2, A3),
	Discard2 = discard_vars(AccessedInElseOnly, Indent + 1),
	Discard3 = discard_vars(AccessedInThenOnly, Indent + 1),
	% TODO Push new local scope to state.
	{Decl1, Code1, RetVar1, State1} = c(E1, State),
	BoolCheck = case T1 of
		boolean ->
			% We know it's boolean.  No runtime check.
			[];
		_ ->
			% Code to ensure it's a boolean.
			[indent(Indent), "lsr_ensure_boolean(", RetVar1, ");\n"]
	end,
	{RetVar, State2} = new_tmpvar(State1, "_if"),
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
c(#expr{body = #binop{op = seq, left = E1, right = E2}},
  State = #state{indent=Indent}) ->
	{Decl1, Code1, DeadVar, State2} = c(E1, State),
	{Decl2, Code2, RetVar, State3} = c(E2, State2),
	FreeCode = case E1 of
		#expr{body = #assign{pat = #expr{body = #var{action = bind}}}} ->
			%% Small optimatization.  A variable has just ben assigned this
			%% (temporary) value.  It can't be the last access.
			[];
		_ ->
			Accesses = lsrtyper:get_accessed(E2),
			case lsrvarsets:is_element(DeadVar, Accesses) of
				false -> [indent(Indent), "lsr_free_unused(", DeadVar, ");\n"];
				true  -> [] % DeadVar will be used later, so refc can't be zero here.
			end
	end,
	{[Decl1, Decl2], [Code1, FreeCode, Code2], RetVar, State3};

% Concatenation
c(#expr{body = #strcat{left  = Left  = #expr{type = TL},
                       right = Right = #expr{type = TR},
                       fixed = true}},
  State = #state{indent = Indent}) ->
	{LeftDecl, LeftCode, LeftVar, State2} = c(Left, State),
	%% If it's not infered to be a string, add a runtime assertion
	AssertLeft = case TL of
		string -> [];
		_      -> [indent(Indent), "lsr_assert_string(", LeftVar, ");\n"]
	end,	
	{RightDecl, RightCode, RightVar, State3} = c(Right, State2),
	AssertRight = case TR of
		string -> [];
		_      -> [indent(Indent), "lsr_assert_string(", RightVar, ");\n"]
	end,
	{RetVar, State4} = new_tmpvar(State3, "_strcat"),
	ConcatCode = [indent(Indent), RetVar, " = lsr_string_concat(", LeftVar, ", ", RightVar, ");\n"],
	{[LeftDecl, RightDecl, decl(RetVar, string, Indent)],
	 [LeftCode, AssertLeft, RightCode, AssertRight, ConcatCode],
	 RetVar,
	 State4};

% Assignment (match) Pattern = Expression, binds any free vars in pattern
c(#expr{body = #assign{pat = Pattern, expr = Expr}, type = ExprType}, 
  State = #state{indent = Indent}) ->
	{ExprDecl, ExprCode, ExprVar, State1} = c(Expr, State),
	{LabelPrefix, State2} = new_label(State1),
	FailLabel = LabelPrefix ++ "_mismatch",
	SuccessLabel = LabelPrefix ++ "_match",
	Subject = #subject{value = ExprVar,
	                   type = ExprType,
	                   faillabel = FailLabel},
	_ = Subject, %% FIXME pass subject to c_match
	{MatchDecl, MatchCode,
	 BindDecl, BindCode, State3} = c_match(Pattern, Subject, State2),
	% Code to assert successed match and raise an error otherwise
	SuccessCode = case MatchCode of
		[] ->
			[];
		_ ->
			[indent(Indent), "goto ", SuccessLabel, ";\n",
			 indent(Indent), FailLabel, ":\n",
			 indent(Indent),
			 "lsr_error(\"Pattern mismatch (", ExprVar, ")\");\n",
			 indent(Indent), SuccessLabel, ":\n"]
	end,
	{[ExprDecl, MatchDecl, BindDecl],
	 [ExprCode, MatchCode, SuccessCode, BindCode],
	 ExprVar,
	 State3};

% Variable access
c(#expr{body = #var{name = Name, action = AccessType}, type = boolean},
  State = #state{indent = Indent}) ->
	% reference counted
	Code = case AccessType of
		lastaccess -> [indent(Indent),
		               "/* last access of boolean '", Name, "' */\n"];
		access     -> []
	end,
	{[], Code, Name, State};
c(#expr{body = #var{name = Name, action = AccessType}, type = Type},
  State = #state{indent = Indent})
			when Type =:= string; Type =:= any ->
	Code = case AccessType of
		lastaccess -> [indent(Indent), "lsr_decref(", Name, ");",
		               " /* last access */\n"];
		access     -> []
	end,
	{[], Code, Name, State};

% Literals, either a plain C value or an initialized tempvar
c(#expr{body = #literal{type = boolean, data = Content}},
  State = #state{indent = Indent}) ->
	% create boxed type and a new tmpvar
	{Name, State2} = new_tmpvar(State, "_bool_lit"),
	Decl = [indent(Indent), c_type(boolean), " ", Name,
	        " = ", c_boolean_literal(Content), ";\n"],
	{Decl, [], Name, State2};
	%--------
	% We may want to use unboxed values, represented directly as a c bool
	%Ret = c_boolean_literal(Content),
	%{[], [], Ret, State};
c(#expr{body = #literal{type = string, data = Content}},
  State = #state{indent=Indent}) ->
	% create boxed type and a new tmpvar
	{Name, State2} = new_tmpvar(State, "_str_lit"),
	Decl = [indent(Indent),
	        c_type(string), " ", Name, " = ",
	        c_string_literal(Content), ";\n"],
	{Decl, [], Name, State2}.

-spec discard_vars(string(), integer()) -> iolist().
discard_vars(Names, Indent) ->
	lists:map(fun (Name) -> [indent(Indent), "lsr_discard_ref(", Name, "); /* never used after this point */\n"] end,
	          Names).

% Returns an unused label
-spec new_label(#state{}) -> {string(), #state{}}.
new_label(State = #state{}) ->
	new_label(State, "").

-spec new_label(#state{}, Suffix::string()) -> {string(), #state{}}.
new_label(State = #state{nextlabel = NextLabel}, Suffix) ->
	{"l" ++ integer_to_list(NextLabel) ++ Suffix,
	 State#state{nextlabel = NextLabel + 1}}.

% Returns an unused temporary variable name (for intermediate values)
-spec new_tmpvar(#state{}) -> {string(), #state{}}.
new_tmpvar(State = #state{}) ->
	new_tmpvar(State, "").

-spec new_tmpvar(#state{}, Suffix::string()) -> {string(), #state{}}.
new_tmpvar(State = #state{nexttmp=NextTmp}, Suffix) ->
	{"tmp" ++ integer_to_list(NextTmp) ++ Suffix,
	 State#state{nexttmp = NextTmp + 1}}.

c_type(_Type) ->
	%case Type of string -> "lsr_t *"; boolean -> "bool" end.
	"lsr_t *".

c_boolean_literal(Content) ->
	C_bool = case Content of true -> "true"; false -> "false" end,
	["lsr_bool_to_ptr(", C_bool, ")"].
c_string_literal(Content) ->
	["lsr_string_literal(", Content, ")"].

% declare and initialize
-spec decl(string(), typename(), integer()) -> iolist().
decl(Name, Type, Indent) ->
	[indent(Indent), c_type(Type), " ", Name, ";",
	 " /* ", atom_to_list(Type), " */\n"].

-spec indent(integer()) -> iolist().
indent(N) when N >= 0 ->
	string:chars($\s, N * 2).

-spec indent_more(#state{}) -> #state{}.
indent_more(State = #state{indent = N}) ->
	State#state{indent = N + 1}.

-spec indent_less(#state{}) -> #state{}.
indent_less(State = #state{indent = N}) ->
	State#state{indent = N - 1}.
