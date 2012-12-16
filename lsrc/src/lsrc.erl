-module(lsrc).

-export([compile/1]).

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

c_prog({prog, E, string, Vars, _OuterVars=[]}) ->
	State = #state{},
	State1 = indent_more(State),
	#state{indent = Indent} = State1,
	{Decl, Code, RetVar, _State2} = c(E, State1),
	Output =
	["#include \"runtime/runtime.h\"\n",
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
	 indent(Indent), "printf(\"%s\\n\", lsr_chars(", RetVar, "));\n",
	 indent(Indent), "lsr_free_unused(", RetVar, ");\n",
	 indent(Indent), "return 0;\n",
	 "}\n"],
	list_to_binary(Output).

c_vardecls(Vars, Indent) ->
	c_vardecls(Vars, Indent, []).
c_vardecls([], _, Acc) ->
	lists:reverse(Acc);
c_vardecls([{Name, Type} | Vars], Indent, Acc) ->
	c_vardecls(Vars, Indent, [decl(Name, Type, Indent) | Acc]).

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
	{Decl0, Code0, RetVar0, State0} = c(E1, indent_more(State)),
	{Decl1, Code1, RetVar1, State1} = case lsrtyper:get_type(E1) of
		boolean ->
			{[], [], RetVar0, State0};
		_ ->
			% boxed
			{BoolVar, StateBool} = new_tmpvar(State0),
			{decl(BoolVar, c_type(boolean), Indent),
			 [indent(Indent), BoolVar, " = lsr_ptr_to_bool(", RetVar0, ");\n"],
			 BoolVar, StateBool}
	end,
	{RetVar, State2} = new_tmpvar(State1),
	{Decl2, Code2, RetVar2, State3} = c(E2, State2),
	{Decl3, Code3, RetVar3, State4} = c(E3, State3),
	State5 = indent_less(State4),
	Decl = [Decl0, Decl1, decl(RetVar, T, Indent)],
	Code = [Code0, Code1,
	        indent(Indent), "if (", RetVar1, ") {\n",
	        Decl2, Discard2, Code2,
	        indent(Indent + 1), RetVar, " = ", RetVar2, ";\n",
	        indent(Indent), "} else {\n", 
	        Decl3, Discard3, Code3,
	        indent(Indent + 1), RetVar, " = ", RetVar3, ";\n",
	        indent(Indent), "}\n"
	        %, "lsr_free_unused(", RetVar1, ");\n"
	       ],
	{Decl, Code, RetVar, State5};

% Sequence: Discard the first value and continue. Keep only the last value. 
c({seq, E1, E2, string, _Accesses}, State = #state{indent=Indent}) ->
	{Decl1, Code1, DeadVar, State2} = c(E1, State),
	{Decl2, Code2, RetVar, State3} = c(E2, State2),
	Accesses = lsrtyper:aexpr_get_accesses(E2),
	FreeCode = case lsrvarsets:is_element(DeadVar, Accesses) of
		false -> [indent(Indent), "lsr_free_unused(", DeadVar, ");\n"];
		true  -> [] % DeadVar will be used later, so refc can't be zero here.
	end,
	{[Decl1, Decl2], [Code1, FreeCode, Code2], RetVar, State3};

% Concatenation
c({concat, Left, Right, string, _Accesses}, State = #state{indent=Indent}) ->
	{LeftDecl, LeftCode, LeftVar, State2} = c(Left, State),
	{RightDecl, RightCode, RightVar, State3} = c(Right, State2),
	{RetVar, State4} = new_tmpvar(State3),
	ConcatCode = [indent(Indent), RetVar, " = lsr_string_concat(", LeftVar, ", ", RightVar, ");\n"],
	{[LeftDecl, RightDecl, decl(RetVar, string, Indent)],
	 [LeftCode, RightCode, ConcatCode],
	 RetVar,
	 State4};

% Assignment
c({assign, {bind, _, VarName, VarType}, Expr, ExprType, _Accesses}, 
  State = #state{indent=Indent}) ->
	%% TODO: Relax the type matching to a subtype relation
	VarType = ExprType,
	{ExprDecl, ExprCode, ExprVar, State2} = c(Expr, State),
	Code = [indent(Indent), VarName, " = ", ExprVar, ";",
	        %"\n",
	        %indent(Indent),
	        " ",
	        "lsr_incref(", VarName, ");\n"],
	{ExprDecl,
	 [ExprCode, Code],
	 VarName,
	 State2};

% Variable access
c({var, _, Name, string, AccessType}, State = #state{indent=Indent}) ->
	Code = case AccessType of
		lastaccess -> [indent(Indent), "lsr_decref(", Name, ");",
		               " /* last access */\n"];
		access     -> []
	end,
	{[], Code, Name, State};

% Literals, either a plain C value or an initialized tempvar
c({literal, {boolean, _, Content}}, State) ->
	% direct translate to c value
	Ret = c_boolean_literal(Content),
	{[], [], Ret, State};
c({literal, {string, _, Content}}, State = #state{indent=Indent}) ->
	% create boxed type and inc
	{Name, State2} = new_tmpvar(State),
	Decl = [indent(Indent), c_type(string), " ", Name, " = ",
	        c_string_literal(Content), ";\n"],
	{Decl, [], Name, State2}.

discard_vars(Names, Indent) ->
	lists:map(fun (Name) -> [indent(Indent), "lsr_discard_ref(", Name, "); /* never used after this point */\n"] end,
	          Names).

% Returns an unused temporary variable name (for intermediate values)
-spec new_tmpvar(#state{}) -> {string(), #state{}}.
new_tmpvar(State = #state{nexttmp=NextTmp}) ->
	{"tmp" ++ integer_to_list(NextTmp), State#state{nexttmp = NextTmp + 1}}.

c_type(Type) ->
	case Type of string -> "lsr_tagged_t *"; boolean -> "bool" end.

c_boolean_literal(Content) ->
	case Content of true -> "true"; false -> "false" end.
c_string_literal(Content) ->
	["(lsr_tagged_t *) LSR_STRING_CONST_PREFIX ", Content].

% declare and initialize
-spec decl(string(), lsrtyper:typename(), integer()) -> iolist().
decl(Name, Type, Indent) ->
	[indent(Indent), c_type(Type), " ", Name, ";\n"].

indent(N) when N >= 0 ->
	string:chars($\s, N * 2).

indent_more(State = #state{indent = N}) ->
	State#state{indent = N + 1}.

indent_less(State = #state{indent = N}) ->
	State#state{indent = N - 1}.
