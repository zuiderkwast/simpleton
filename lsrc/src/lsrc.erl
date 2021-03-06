-module(lsrc).

-export([compile/1, compile/2]).

-include("types.hrl").

-record(state, {nexttmp=1, nextlabel=1, indent=0, globalscope}).

-define(FUNPREFIX, "fun_").

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
compile(String, Verbose) ->
	try
		{ok, Tokens} = lsrlexer:tokenize(String),
		Verbose andalso io:format("Tokens:~n~p~n", [Tokens]),
		{ok, Tree} = lsrparser:parse(Tokens),
		Verbose andalso io:format("Tree:~n~p~n", [Tree]),
		{ok, Annotated} = lsrtyper:annotate(Tree),
		Verbose andalso io:format("Annotated tree:~n~p~n", [Annotated]),
		{ok, c_module(Annotated, Verbose)}
	catch
		error:{badmatch, {error, Error}} ->
			Message1 = case Error of
				{{Line, Col}, lsrparser, Message} ->
					io_lib:format("~s on line ~p, column ~p",
					              [Message, Line, Col]);
				{lsrtyper, Message} ->
					["Type: ", Message];
				Other ->
					["Other: ", io_lib:format("~p", [Other])]
			end,
			{error, Message1}
	end.

c_module(Module = #module{defs = Defs, types = Scope}, MemDebug) ->
	% 1. Compile all Defs.
	FunDeclsAndDefs = [c_fundef(Name, Funs, #state{globalscope = Scope}) ||
	                   {Name, Funs} <- Defs],
	DeclsC = [Decl || {Decl, _} <- FunDeclsAndDefs],
	DefsC  = [Code || {_, Code} <- FunDeclsAndDefs],
	MainC = case orddict:find("main", Defs) of
		{ok, Def} -> c_main(Def);
		error -> []
	end,
	Output =
	[case MemDebug of
		true -> "#define LSR_MONITOR_ALLOC\n";
		false -> []
	 end,
	 "#include \"runtime/runtime.h\"\n",
	 "\n",
	 DeclsC,
	 "\n",
	 DefsC,
	 "\n",
	 MainC],
	% 2. If there's a "main", create a main c function
	list_to_binary(Output).

c_main(Def) ->
	["/* main() todo */"].

-spec c_fundef(Name::string(), Funs::funs()) -> {FuncProto::iolist(); FuncDef::iolist()}.
c_fundef(Name, Funs = #funcons{head = #'fun'{numparams = Arity}}, State) ->
	%% 1. Create param var names param1, ..., paramN
	%% 2. Compile a multi-pattern case construct, matched against the params.
	ParamNames = lists:map(fun(N) -> "param" ++ integer_to_list(N) end, lists:seq(1, Arity)),
	ParamDecls = lists:map(fun(Name) -> "lsr_t * " ++ Name end, ParamNames),
	Head = ["lsr_t * ", ?FUNPREFIX, Name, "(", string:join(", ", ParamDecls), ")"],
	ClausesCode = c_funclauses(Name, Funs, ParamNames, indent_more(State)),
	%% TODO
	%% * Funktion börjar, ta emot params
	%% * För varje clause:
	%%   * För varje param:
	%%     * Skapa #subject{value=<paramvar>, type=any, faillabel = <nästa clause>}
	%%   * Om alla patterns matchar:
	%%     * Discard vars not used in this clause (but there aren't any)
	%%     * Bind variabler i params-patterns
	%%     * Kör Body
	%%     * Sätt return-variabel (returnvar)
	%%     * Hoppa till return-label (returnlabel)
	%%   * Annars:
	%%     * Hoppa till nästa clause
	%%   * Om ingen clause matchar -> error
	%% * Frigör params
	%% * Return
	todo.

c_funclauses(Name, nil, _, _) ->
	[indent(1),
	 "lsr_error(\"No matching function clause: ", Name, "\");\n"],
c_funclauses(Name, #funcons{head = Fun, tail = Funs}, ParamNames,
             State = #state{indent = Indent}) ->
	{NextLabel, State1} = new_label(State, "next_funclause"),
	{FunCode, State2} = c_fun(Fun, ParamNames, NextLabel, State1),
	[FunCode,
	 indent(Indent - 1), NextLabel, ":\n" |
	 c_funclauses(Name, Funs, ParamNames, State2)].

c_fun(Fun = #'fun'{params = ParamPatterns, body = BodyExpr},
      ParamValues, FailLabel, State) ->
	Subjects = lists:map(fun(ParamValue) -> #subject{value = ParamValue,
	                                                 type = any,
	                                                 faillabel = FailLabel}
	                     end,
	                     ParamValues),
	{MatchDecl, MatchCode, BindDecl, BindCode, State1} =
		c_match_each(ParamPatterns, Subjects, State),
	%,

%% matches each pattern against the corresponding subject
-spec c_match_each(exprs(), [#subject{}], #state{}) ->
	{iolist(), iolist(), iolist(), iolist(), state()}.
c_match_each(nil, [], State) ->
	{[], [], [], [], State};
c_match_each(#cons{head = Pattern, tail = Patterns, accessed = _A},
             [Subj | Subjs], State) ->
	{MatchDecl, MatchCode,
	 BindDecl, BindCode, State1} = c_match(Pattern, Subj, State),
	{MatchDecls, MatchCodes,
	 BindDecls, BindCodes, State2} = c_match_each(Patterns, Subjs, State1),
	{[MatchDecl | MatchDecls], [MatchCode | MatchCodes],
	 [BindDecl | BindDecls], [BindCode | BindCodes], State2}.

%% For comparison: this is the #'case'{} part of c/2
c_case(#expr{body = #'case'{test = Subj = #expr{type = SubjType}, rules = Rules},
             type = RetType},
       State = #state{indent = Indent}) ->
	{Decl1, Code1, SubjVar, State1} = c(Subj, State),
	Subject = #subject{value = SubjVar,
	                   type = SubjType},
	{AfterLabel, State2} = new_label(State1, "_after_case"),
	{RetVar, State3} = new_tmpvar(State2, "_case_result"),
	RetDecl = decl(RetVar, RetType, Indent),
	{Decl2, Code2, State4} = c_rules(Rules, Subject,
	                                 RetVar, AfterLabel, State3),
	Code3 = [indent(Indent - 1), AfterLabel, ":\n"],
	{[Decl1, RetDecl, Decl2],
	 [indent(Indent), "/* case subject (", SubjVar, ") */\n",
	  Code1,
	  indent(Indent), "/* case ", SubjVar, " of ... */\n",
	  Code2,
	  Code3],
	 RetVar,
	 State4}.


c_prog(Expr = #expr{type = Type, accessed = _OuterVars = []},
       MemDebug) ->
	State = #state{},
	State1 = #state{indent = Indent} = indent_more(State),
	{Decl, Code, RetVar, _State2} = c(Expr, State1),
	Output =
	[case MemDebug of
		true -> "#define LSR_MONITOR_ALLOC\n";
		false -> []
	 end,
	 "#include \"runtime/runtime.h\"\n",
	 "int main() {\n",
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


%% @doc Generate code for incrementing the reference-counter, expept if we know
%% by the type that it's not reference-counted. Takes a type or an expr for
%% convenience.
-spec incref(VarName::string(), typename() | expr()) -> iolist().
incref(_, #expr{body = #literal{type = string}}) -> []; %% string literals
incref(VarName, #expr{type = T}) -> incref(VarName, T); %% any other expr
incref(_, boolean) -> [];
incref(VarName, _) -> [" lsr_incref(", VarName, ");"].


%% @doc Check a value (c variable) against a pattern.
-spec c_match(Pattern::#expr{}, #subject{}, #state{}) ->
      {MatchDecl::iolist(), MatchCode::iolist(),
       BindDecl::iolist(), BindCode::iolist(), #state{}}.
c_match(#expr{body=#var{action=bind, name=VarName}, type=VarType},
        #subject{value=Value, offset=none, length=none},
        State = #state{indent=Indent}) ->
	% Assignment code, increment ref-counter
	BindCode = [indent(Indent), VarName, " = ", Value, ";",
	            incref(VarName, VarType), "\n"],
	{[], [],	 [], BindCode, State};
c_match(#expr{body=#var{action=bind, name=VarName}},
        #subject{value=Value, type=Type, offset=Offset, length=Length},
        State = #state{indent=Indent})
		when Offset =/= none, Length =/= none
		     %(Type =:= string or Type =:= array)
		     ->
	% Assignment code, increment ref-counter
	SliceFun = case Type of
		string -> "lsr_substr";
		array  -> "lsr_array_slice"
	end,
	BindCode = [indent(Indent), VarName, " = ",
	            SliceFun, "(", Value, ", ", Offset, ", ", Length, ");",
	            " lsr_incref(", VarName, ");\n"],
	{[], [],	 [], BindCode, State};
c_match(#expr{body=#var{action=discard}}, _, State) ->
	{[], [],	 [], [], State};

% Expression as pattern: compare equal to the pattern subject
c_match(Expr = #expr{body=Body}, Subject = #subject{}, State)
		when is_record(Body, var);
		     is_record(Body, literal) ->
	% Compile the expression
	{Decl, Code, ExprResult, State2} = c(Expr, State),
	EqCode = c_equality_check(ExprResult, Subject, State2),
	{Decl, [Code, EqCode], [], [], State2};

% Array decomposition
c_match(#expr{body = #array{length = Length, elems = Elems}, type = array},
        Subject = #subject{value=Value, type=ValueType, faillabel=FailLabel,
                           offset=none, length=none},
        State = #state{indent = Indent}) ->
	TypeCheck = case ValueType of
		array -> [];
		_     -> [indent(Indent),
		          "if (!lsr_is_array(", Value, "))",
		          " goto ", FailLabel, ";\n"]
	end,
	LenCheck = [indent(Indent),
	            "if (lsr_array_len(", Value, ") != ", integer_to_list(Length), ")",
	            " goto ", FailLabel, ";\n"],
	{MatchDecl, MatchCode, BindDecl, BindCode, State1} =
		c_match_array_elems(Elems, Subject, 0, State),
	{MatchDecl, [TypeCheck, LenCheck, MatchCode], BindDecl, BindCode, State1};

% Array decomposition of a slice of the subject (array)
c_match(#expr{body = #array{length = Length, elems = Elems}, type = array},
        Subject = #subject{value=Value, type=array, faillabel=FailLabel,
                           offset=OffsetVar, length=LengthVar},
        State = #state{indent = Indent}) ->
	{MatchDecl, MatchCode, BindDecl, BindCode, State1} =
		c_match_array_elems(Elems, Subject, 0, State),
	{MatchDecl, MatchCode, BindDecl, BindCode, State1};

%% String concatenation as pattern. Assert string, find subject's length and
%% skip to helper.
c_match(Expr = #expr{body = #strcat{}, type = string},
        Subject = #subject{value=Value, type=ValueType, faillabel=FailLabel,
                           offset=none, length=none},
        State = #state{indent=Indent}) ->
	Offset = "0",
	TypeCheck = case ValueType of
		string -> [];
		_      -> [indent(Indent),
		           "if (!lsr_is_string(", Value, ")) goto ", FailLabel, ";\n"]
	end,
	{LenDecl, LenCode, LenVar, State1} = c_strlen(Value, State),
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
	{Decl2, Code2, CheckValLen, State2} = c_strlen(CheckVal, State1),

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
	 BindDecl, BindCode, State4};

%% Array concatenation as pattern. Assert array, find subject's length and
%% skip to helper.
c_match(Expr = #expr{body = #arrcat{}, type = array},
        Subject = #subject{value=Value, type=ValueType, faillabel=FailLabel,
                           offset=none, length=none},
        State = #state{indent=Indent}) ->
	Offset = "0",
	TypeCheck = case ValueType of
		array -> [];
		_      -> [indent(Indent),
		           "if (!lsr_is_array(", Value, "))"
		           " goto ", FailLabel, ";\n"]
	end,
	{LenDecl, LenCode, LenVar, State1} = c_arrlen(Value, State),
	Subject1 = Subject#subject{offset=Offset, length=LenVar, type=array},
	{Decl, Code, BindDecl, BindCode, State2} = c_match(Expr, Subject1, State1),
	{[LenDecl, Decl], [TypeCheck, LenCode, Code], BindDecl, BindCode, State2};

%% @doc Match a slice of an array. The type (array) is already checked.
%% Len and Offset are C expressions (normally variable names)
c_match(#expr{body = #arrcat{left = L, right = R}},
        Subject = #subject{faillabel=FailLabel,
                           offset=Offset, length=Len},
        State = #state{indent = Indent})
		when Offset =/= none, Len =/= none ->

	%% Decide sides
	{FixedPat, RestPat, FixedSide} = case lsrtyper:is_fixed(L) of
		true  -> {L, R, left};
		false -> {R, L, right}
	end,

	%% We know FixedPat is a pattern with a fixed length, but there are three
	%% cases:
	%%   1. A bound variable, fixed but unknown length
	%%   2. An array pattern, known length, but may contain free variables
	%%      (This case doesn't exist for strings, except maybe future regex)
	%%   3. Another array concatenation where both operands are fixed.
	{Code1, FixedValLen} = c_array_length(FixedPat, FailLabel, Indent),

	%% Split Subject into FixedSubject and RestSubject
	{Decl1, Code2, FixedSubject, RestSubject, State1} =
		split_subject(Subject, FixedSide, FixedValLen, State),

	%% Total length check (unless they are identical)
	#subject{length = FixedSubjLen} = FixedSubject,
	Code3 = case FixedValLen =:= FixedSubjLen of
		true  -> [];
		false -> [indent(Indent), "if (", FixedValLen, " != ", FixedSubjLen, ")"
		          " goto ", FailLabel, ";\n"]
	end,

	%% Match the fixed length pattern
	{Decl2, Code4, BindFixedDecl, BindFixedCode, State2} =
		c_match(FixedPat, FixedSubject, State1),

	%% Match rest
	{Decl3, Code5, BindRestDecl, BindRestCode, State3} =
		c_match(RestPat, RestSubject, State2),

	{[Decl1, Decl2, Decl3],
	 [Code1, Code2, Code3, Code4, Code5],
	 [BindFixedDecl, BindRestDecl], [BindFixedCode, BindRestCode], State3}.

%% @doc Computes the length of an array expression that consists of array
%% constuctors, array concatenation and bound variables. Returns a C expression
%% for the length.
-spec c_array_length(expr(), FailLabel::string(), Indent::integer()) ->
	{Code::iolist(), LengthExpr::iolist()}.
c_array_length(#expr{body = #var{name = Name, action = A}, type = Type},
               FailLabel, Indent)
		when A =:= access; A =:= lastaccess ->
	Code = case Type of
		array -> [];
		_     -> [indent(Indent),
		          "if (!lsr_is_array(", Name, "))"
		          " goto ", FailLabel, ";\n"]
	end,
	{Code, ["lsr_array_len(", Name, ")"]};
c_array_length(#expr{body = #array{length = Length}}, _, _) ->
	{[], integer_to_list(Length)};
c_array_length(#expr{body = #arrcat{left = L, right = R, fixed = true}},
               FailLabel, Indent) ->
	{CodeL, LengthL} = c_array_length(L, FailLabel, Indent),
	{CodeR, LengthR} = c_array_length(R, FailLabel, Indent),
	{[CodeL, CodeR], [LengthL, " + ", LengthR]}.

-spec c_match_array_elems(exprs(), Subject::#subject{}, Pos::integer, #state{}) ->
	{MatchDecl::iolist(), MatchCode::iolist(),
	 BindDecl::iolist(), BindCode::iolist(), #state{}}.
c_match_array_elems(nil, _, _, State) ->
	{[], [], [], [], State};
c_match_array_elems(#cons{head=HeadPat, tail=TailPat},
                    Subject = #subject{value=Value,
                                       offset=OffsetVar,
                                       faillabel=FailLabel},
                    Pos, State = #state{indent=Indent}) ->
	{HeadValue, State1} = new_tmpvar(State, "_array_elem"),
	HeadType = any,
	HeadDecl = [indent(Indent), c_type(HeadType), " ", HeadValue, ";\n"],
	SubjectPos = case OffsetVar of
		none -> integer_to_list(Pos);
		Var  -> [Var, " + ", integer_to_list(Pos)]
	end,
	HeadCode = [indent(Indent), HeadValue, " = ",
	            "lsr_array_get(", Value, ", ", SubjectPos, ");\n"],
	HeadSubject = #subject{value=HeadValue, type=HeadType, faillabel=FailLabel},
	{MatchDecl, MatchCode, BindDecl, BindCode, State2} =
		c_match(HeadPat, HeadSubject, State1),
	{MatchDecls, MatchCodes, BindDecls, BindCodes, State3} =
		c_match_array_elems(TailPat, Subject, Pos + 1, State2),
	{[HeadDecl, MatchDecl | MatchDecls],
	 [HeadCode, MatchCode | MatchCodes],
	 [BindDecl | BindDecls],
	 [BindCode | BindCodes],
	 State3}.

%% Split a pattern subject into a check part and a match part.
-spec split_subject(#subject{}, left | right, string(), #state{}) ->
	{Decl::iolist(), Code::iolist(), #subject{}, #subject{}, #state{}}.
split_subject(Subject = #subject{offset=Offset, length=Len},
              left, CheckLen,
              State = #state{indent=Indent}) ->
	{RestLen, State1} = new_tmpvar(State, "_rest_length"),
	Decl1 = [indent(Indent), "size_t ", RestLen, ";\n"],
	Code1 = [indent(Indent), RestLen, " = ", Len, " - (", CheckLen, ");\n"],
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
	Code1 = [indent(Indent), RestLen, " = ", Len, " - (", CheckLen, ");\n"],
	{CheckOffset, State2} = new_tmpvar(State1, "_fixed_offset"),
	Decl2 = [indent(Indent), "size_t ", CheckOffset, ";\n"],
	Code2 = [indent(Indent), CheckOffset, " = ", Offset, " + ", RestLen, ";\n"],
	{[Decl1, Decl2],
	 [Code1, Code2],
	 Subject#subject{offset = CheckOffset, length = CheckLen},
	 Subject#subject{length = RestLen},
	 State2}.

%% @doc Create C code for an equality check with goto faillabel on mismatch.
%% Helper for pattern matching.
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
		array  -> "lsr_equals_slice"
	end,
	[indent(Indent), "if (!", EqFun, "(", CheckVar, ", ", Value, ", ",
	 Offset, ", ", Length, ")) ", "goto ", FailLabel, ";\n"].


%% @doc C code for the length of a string value
-spec c_strlen(Value::iolist(), #state{}) ->
	{Decl::iolist(), Code::iolist(), RetVar::string(), State::#state{}}.
c_strlen(Value, State) ->
	c_length_helper("lsr_strlen", Value, State).

%% @doc C code for the length of an array value
-spec c_arrlen(Value::string(), #state{}) ->
	{Decl::iolist(), Code::iolist(), RetVar::string(), State::#state{}}.
c_arrlen(Value, State) ->
	c_length_helper("lsr_array_len", Value, State).

-spec c_length_helper(LenFunName::string(), Value::string(), #state{}) ->
	{Decl::iolist(), Code::iolist(), RetVar::iolist(), State::#state{}}.
c_length_helper(LenFunName, Value, State = #state{indent = Indent}) ->
	{Var, State1} = new_tmpvar(State, "_length"),
	Decl = [indent(Indent), "size_t ", Var, ";\n"],
	Code = [indent(Indent), Var, " = ", LenFunName, "(", Value, ");\n"],
	{Decl, Code, Var, State1}.

c_rules(nil, #subject{value = Value},
        _RetVar, AfterLabel,
        State = #state{indent = Indent}) ->
	Code = [indent(Indent),
	        "lsr_error(\"No case matches value (", Value, ")\");\n",
	        indent(Indent - 1), AfterLabel, ":\n"],
	{[],	 Code, State};
c_rules(#rulecons{head = #rule{pat = Pattern, expr = Body}, tail = Tail},
        Subject = #subject{},
        RetVar, AfterLabel,
        State = #state{indent = Indent}) ->
	{NextLabel, State1} = new_label(State, "next_case"),
	Subject1 = Subject#subject{faillabel = NextLabel},
	{MatchDecl, MatchCode,
	 BindDecl, BindCode, State2} = c_match(Pattern, Subject1, State1),
	%% Free vars not used anymore if this cases matches
	VarsUnusedInBody = ordsets:subtract(lsrtyper:rules_get_accessed(Tail),
	                                    lsrtyper:get_accessed(Body)),
	DiscardCode = discard_vars(VarsUnusedInBody, Indent + 1),
	{BodyDecl, BodyCode, BodyRetVar, State3} = c(Body, indent_more(State2)),
	BodyCode2 =
		[indent(Indent), "{ /* case matched */\n",
		 indent(Indent + 1), "/* case tmp vars */\n", BodyDecl,
		 indent(Indent + 1), "/* discard vars not used in this branch */\n", DiscardCode,
		 indent(Indent + 1), "/* case code */\n", BodyCode,
		 indent(Indent + 1), RetVar, " = ", BodyRetVar, ";\n",
		 indent(Indent), "}\n"],
	State4 = indent_less(State3),
	{AfterDecl, AfterCode, State6} = case MatchCode of
		[] ->
			%% No match code means this case always matches. Skip the rest of the cases.
			%% This is an optimization only.
			{[],
			 [indent(Indent), "/* the previous case always matches, so no more cases here. */\n"],
			 State4};
		_ ->
			%% Discard vars used only in the pattern and expr that didn't match
			VarsUsusedAfterCase =
				ordsets:subtract(ordsets:union(lsrtyper:get_accessed(Body),
				                               lsrtyper:get_accessed(Pattern)),
				                 lsrtyper:rules_get_accessed(Tail)),
			DiscardOnMismatchCode = discard_vars(VarsUsusedAfterCase, Indent),
			%% Recursively handle the rest of the cases
			{TailDecl, TailCode, State5} = c_rules(Tail, Subject, RetVar, AfterLabel, State4),
			{TailDecl,
			 [indent(Indent), "goto ", AfterLabel, ";\n",
			  indent(Indent - 1), NextLabel, ":\n",
			  indent(Indent), "/* discard vars used only in the previous case */\n",
			  DiscardOnMismatchCode,
			  TailCode],
			 State5}
	end,
	{[MatchDecl, BindDecl, AfterDecl],
	 [indent(Indent), "/* case pattern test */\n", MatchCode,
	  indent(Indent), "/* case matched; bind vars in pattern */\n", BindCode,
	  BodyCode2, AfterCode],
	 State6}.


-spec c(#expr{}, #state{}) -> {iolist(), iolist(), iolist(), #state{}}.

c(#expr{body = #do{expr = E, locals = Vars}, type = Type},
  State = #state{indent = Indent0}) ->
	{RetVar, State1} = new_tmpvar(State, "_do"),
	DoDecl = decl(RetVar, Type, Indent0),
	State2 = #state{indent = Indent} = indent_more(State1),
	{Decl, Code, RetVar1, State3} = c(E, State2),
	DoCode =
	[indent(Indent0), "{ /* do */\n",
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
	 indent(Indent), RetVar, " = ", RetVar1, ";\n",
	 indent(Indent0), "}\n"],
	{DoDecl, DoCode, RetVar, State3};

c(#expr{body = #'if'{'cond' = E1 = #expr{type = T1},
                     'then' = E2 = #expr{accessed = A2},
                     'else' = E3 = #expr{accessed = A3}},
        type = T},
  State = #state{indent=Indent}) ->
	% TODO Free the input values (i.e. the condition).
	% Free all vars in the dead branch which would be last accessed there.
	AccessedInElseOnly = ordsets:subtract(A3, A2),
	AccessedInThenOnly = ordsets:subtract(A2, A3),
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
	        indent(Indent), "if (lsr_bool_value(", RetVar1, ")) {\n",
	        Decl2, Discard2, Code2,
	        indent(Indent + 1), RetVar, " = ", RetVar2, ";\n",
	        indent(Indent), "} else {\n", 
	        Decl3, Discard3, Code3,
	        indent(Indent + 1), RetVar, " = ", RetVar3, ";\n",
	        indent(Indent), "}\n"
	       ],
	{Decl, Code, RetVar, State5};

c(#expr{body = #'case'{test = Subj = #expr{type = SubjType}, rules = Rules},
        type = RetType},
  State = #state{indent = Indent}) ->
	{Decl1, Code1, SubjVar, State1} = c(Subj, State),
	Subject = #subject{value = SubjVar,
	                   type = SubjType},
	{AfterLabel, State2} = new_label(State1, "_after_case"),
	{RetVar, State3} = new_tmpvar(State2, "_case_result"),
	RetDecl = decl(RetVar, RetType, Indent),
	{Decl2, Code2, State4} = c_rules(Rules, Subject,
	                                 RetVar, AfterLabel, State3),
	Code3 = [indent(Indent - 1), AfterLabel, ":\n"],
	{[Decl1, RetDecl, Decl2],
	 [indent(Indent), "/* case subject (", SubjVar, ") */\n",
	  Code1,
	  indent(Indent), "/* case ", SubjVar, " of ... */\n",
	  Code2,
	  Code3],
	 RetVar,
	 State4};

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
			case ordsets:is_element(DeadVar, Accesses) of
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

c(#expr{body = #arrcat{left  = Left  = #expr{type = TL},
                       right = Right = #expr{type = TR},
                       fixed = true}},
  State = #state{indent = Indent}) ->
	{LeftDecl, LeftCode, LeftVar, State2} = c(Left, State),
	%% If it's not infered to be an array, add a runtime assertion
	AssertLeft = case TL of
		array -> [];
		_     -> [indent(Indent), "lsr_assert_array(", LeftVar, ");\n"]
	end,	
	{RightDecl, RightCode, RightVar, State3} = c(Right, State2),
	AssertRight = case TR of
		array -> [];
		_     -> [indent(Indent), "lsr_assert_array(", RightVar, ");\n"]
	end,
	{RetVar, State4} = new_tmpvar(State3, "_arrcat"),
	ConcatCode = [indent(Indent), RetVar, " = lsr_array_concat(", LeftVar,
	              ", ", RightVar, ");\n"],
	{[LeftDecl, RightDecl, decl(RetVar, array, Indent)],
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
	{MatchDecl, MatchCode,
	 BindDecl, BindCode, State3} = c_match(Pattern, Subject, State2),
	% Code to assert successed match and raise an error otherwise
	SuccessCode = case MatchCode of
		[] ->
			[];
		_ ->
			[indent(Indent), "goto ", SuccessLabel, ";\n",
			 indent(Indent - 1), FailLabel, ":\n",
			 indent(Indent),
			 "lsr_error(\"Pattern mismatch (", ExprVar, ")\");\n",
			 indent(Indent - 1), SuccessLabel, ":\n"]
	end,
	{[ExprDecl, MatchDecl, BindDecl],
	 [ExprCode, MatchCode, SuccessCode, BindCode],
	 ExprVar,
	 State3};

c(#expr{body = #array{length = Length, elems = Elems}, type = array},
  State = #state{indent = Indent}) ->
  	{RetVar, State1} = new_tmpvar(State, "_array"),
	InitDecl = [indent(Indent), c_type(array), " ", RetVar, ";\n"],
	InitCode = [indent(Indent), RetVar, " = ",
	            "lsr_array_create(", integer_to_list(Length), ");\n"],
	State2 = indent_more(State1),
	{ElemsDecl, ElemsCode, State3} = c_array_elems(Elems, RetVar, 0, State2),
	State4 = indent_less(State3),
	{InitDecl,
	 [InitCode,
	  indent(Indent), "{ /* populate array */\n",
	  ElemsDecl,
	  ElemsCode,
	  indent(Indent), "}\n"],
	 RetVar, State4};

% Variable access
c(#expr{body = #var{name = Name, action = AccessType}, type = Type},
  State = #state{indent = Indent}) ->
	Code = case AccessType of
		lastaccess ->
			[indent(Indent),
			 case Type of
				boolean -> ["/* last access of boolean ", Name, " */"];
				_       -> ["lsr_decref(", Name, "); /* last access */"]
			 end,
			 "\n"];
		access -> []
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
c(#expr{body = #literal{type = string, data = Content}},
  State = #state{indent=Indent}) ->
	% create boxed type and a new tmpvar
	{Name, State2} = new_tmpvar(State, "_str_lit"),
	Decl = [indent(Indent),
	        c_type(string), " ", Name, " = ",
	        c_string_literal(Content), ";\n"],
	{Decl, [], Name, State2}.

-spec c_array_elems(exprs(), ArrayVar::string(), Pos::integer(), #state{}) ->
	{Decl::iolist(), Code::iolist(), #state{}}.
c_array_elems(nil, _ArrayVar, _Pos, State) ->
	{[], [], State};
c_array_elems(#cons{head = Head, tail = Tail}, ArrayVar, Pos,
        State = #state{indent = Indent}) ->
	{Decl,  Code,  HeadVar, State1} = c(Head, State),
	{Decls, Codes, State2} = c_array_elems(Tail, ArrayVar, Pos + 1, State1),
	InsCode = [indent(Indent), "lsr_array_set(", ArrayVar, ", ",
	           integer_to_list(Pos), ", ", HeadVar, ");", incref(HeadVar, Head),
	           "\n"],
	{[Decl | Decls], [Code, InsCode | Codes], State2}.

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

%% @doc Known type to runtime type. If we want unboxed values in specific cases,
%% we could do it here.
c_type(_Type) ->
	%case Type of string -> "lsr_t *"; boolean -> "bool" end.
	"lsr_t *".

c_boolean_literal(Content) ->
	C_bool = case Content of true -> "true"; false -> "false" end,
	["lsr_create_bool(", C_bool, ")"].
c_string_literal(Content) ->
	["lsr_string_literal(", Content, ")"].

% declare
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
