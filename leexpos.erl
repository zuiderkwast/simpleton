%% @doc A wrapper for leex to add line and column numbers to tokens.
%% @TODO: Finish this shit.........
%% @author Viktor Söderqvist
%% @licence MIT
-module(leexpos).

-type token() :: {atom(), {integer(), integer()}} |
                 {atom(), {integer(), integer()}, any()}.
-export_type([token/0]).

-export([string/2]).

-record(state, {module, lines, curlineno, curcolno, cont, contlineno,
                contcolno, acc}).


%% @doc A wrapper for Module:string/1, where Module is a leex generated lexer.
%%      Returns the same tokens as Module:string/1 would, except that the line
%%      numbers are replaced with a {Line, Column} tuple.
-spec string(Module::atom(), String::iodata()) ->
	{ok, [Token], {EndLine::integer(), EndCol::integer()}} | ErrorInfo.
string(Module, String) ->
	string(Module, String, StartLine).

%% @doc A wrapper for Module:string/1, where Module is a leex generated lexer.
string(Module, String, StartLine) ->
	Lines = re:split(String, "(\r\n|\r|\n)", [{return, list}]),
	%io:format("Lines: ~p~n", [Lines]),
	State = #state{lines = Lines, curlineno = StartLine, curcolno = 1,
	               cont = [], contlineno = StartLine, contcolno = 1, acc = []},
	tokenize_lines(State).
	%{ok, Tokens}.


-spec tokenize_lines(#state{}) ->
	{ok, [token()], {EndLine::integer(), EndCol::integer()}}.
-spec tokenize_lines(LinesInQueue::[string()], CurrentLine::string(),
                     CurrentLine::integer(), CurrentCol::integer(),
                     PartialToken::{tuple() | [], integer(), integer()},
                     Acc::[token()]) -> {ok, [token()]}.
tokenize_lines([Nl | Lines], [], LineNo, _ColNo, Cont, Acc)
		when Nl =:= "\r\n"; Nl =:= "\r"; Nl =:= "\n" ->
	%% Newline. Update line and col and add the newline to the buffer.
	tokenize_lines(Lines, Nl, LineNo + 1, 0, Cont, Acc);
tokenize_lines([Line | Lines], [], LineNo, ColNo, Cont, Acc) ->
	%% Char buffer empty. Add the next line to buffer.
	tokenize_lines(Lines, Line, LineNo, ColNo, Cont, Acc);
tokenize_lines([], [], LineNo, ColNo, {Cont, ContLine, ContCol}, Acc) ->
	%% No more lines. Send eof to the lexer.
	Acc1 = case token(Cont, eof) of
		{done, {eof, _}, _} -> Acc;
		{done, Token, eof} -> add_token(Token, ContLine, ContCol, Acc)
	end,
	{ok, lists:reverse(Acc1), {LineNo, ColNo}};

tokenize_lines(Lines, Chars, LineNo, ColNo, {Cont, ContLine, ContCol}, Acc) ->
	TokenRet = case Cont of
		[] -> token([], Chars, LineNo);
		_  -> token(Cont, Chars, ContLine)
	end,
	case TokenRet of
		{more, Cont1} ->
			%% Continue on the next line
			tokenize_lines(Lines, [], LineNo, ColNo,
			               {Cont1, ContLine, ContCol}, Acc);
		{done, Token, RestChars} ->
			Acc1 = add_token(Token, ContLine, ContCol, Acc),
			%% @TODO: Handle tabs as 1-8 chars, depending on position
			ColNo1 = ColNo + length(Chars) - length(RestChars),
			tokenize_lines(Lines, RestChars, LineNo, ColNo1,
			               {[], LineNo, ColNo1}, Acc1)
	end.

%% @doc Add line and col data to token and prepends to Acc (list of tokens)
add_token({ok, skip_token, _EndLine}, _Line, _Col, Acc) ->
	Acc;
add_token({ok, Token, _EndLine}, Line, Col, Acc) ->
	Token1 = case Token of
		{Tag, Line} when is_atom(Tag) -> {Tag, {Line, Col}};
		{Tag, Line, Data}             -> {Tag, {Line, Col}, Data}
	end,
	[Token1 | Acc].
