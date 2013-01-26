Definitions.



Rules.

%% Literals. Token on the form {Type, Content}.
"[^"\\]*"  : {token, {string, TokenChars}}.
true|false : {token, {boolean, list_to_atom(TokenChars)}}.

%% Keywords. Token on the form atom().
if|then|else|case|of : {token, list_to_atom(TokenChars)}.

%% Indentifiers. Token on the form {ident, string()}.
[a-zA-Z_][a-zA-Z0-9_]* : {token, {ident, TokenChars}}.

%% Various operators on the form atom() as the token.
~       : {token, '~'}.
@       : {token, '@'}.
\+      : {token, '+'}.
\*      : {token, '*'}.
=       : {token, '='}.
;       : {token, ';'}.
\(      : {token, '('}.
\)      : {token, ')'}.

\[      : {token, '['}.
\]      : {token, ']'}.
,       : {token, ','}.
->      : {token, '->'}.

%% Skip comments and whitespace, by returning a token 'skip_token'
#[^\n]*    : {token, skip_token}.
[\s\n\r\t] : {token, skip_token}.

Erlang code.

-type token() :: {atom(), {integer(), integer()}} |
                 {atom(), {integer(), integer()}, any()}.
-export_type([token/0]).
-export([tokenize/1]).

%% @doc Splits source code into tokens. This is the function that should be used
%% since it adds line and column numbers and inserts semicolons and brackes by
%% indentation.
-spec tokenize(iodata()) -> [token()].
tokenize(String) ->
	Lines = re:split(String, "(\r\n|\r|\n)", [{return, list}]),
	%io:format("Lines: ~p~n", [Lines]),
	{ok, Tokens} = tokenize_lines(Lines, [], 1, 1, {[], 1, 1}, []),
	Tokens1 = process_layout(Tokens),
	{ok, Tokens1}.

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
tokenize_lines([], [], _LineNo, _ColNo, {Cont, ContLine, ContCol}, Acc) ->
	%% No more lines. Send eof to the lexer.
	Acc1 = case token(Cont, eof) of
		{done, {eof, _}, _} -> Acc;
		{done, Token, eof} -> add_token(Token, ContLine, ContCol, Acc)
	end,
	{ok, lists:reverse(Acc1)};

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
			%% TODO: Handle tabs as 1-8 chars, depending on position
			ColNo1 = ColNo + length(Chars) - length(RestChars),
			tokenize_lines(Lines, RestChars, LineNo, ColNo1,
			               {[], LineNo, ColNo1}, Acc1)
	end.

%% @doc Add line and col data to token and prepends to Acc (list of tokens)
add_token({ok, skip_token, _EndLine}, _Line, _Col, Acc) ->
	Acc;
add_token({ok, Token, _EndLine}, Line, Col, Acc) ->
	Token1 = case Token of
		Tag when is_atom(Tag) -> {Tag, {Line, Col}};
		{Tag, Data}           -> {Tag, {Line, Col}, Data}
	end,
	[Token1 | Acc].

%% @doc Fetch the line and column from a token.
get_line_col({_, {Line, Col}}) -> {Line, Col};
get_line_col({_, {Line, Col}, _}) -> {Line, Col}.

%% @doc Inserts extra tokens (brackets and semicolons) based on indentation.
-spec process_layout([token()]) -> [token()].
process_layout(Tokens) ->
	RootIndent = -1,
	process_layout(Tokens, true, [RootIndent], []).

process_layout([], _, IndentStack, Acc) ->
	LastLine = case Acc of
		[LastToken | _] ->
			case LastToken of
				{_, {Line, _}, _} -> Line;
				{_, {Line, _}}    -> Line
			end;
		[] -> 1
	end,
	{ExtraTokens, _IndentStack1} = tokens_before(LastLine + 1, 0, IndentStack),
	lists:reverse(ExtraTokens ++ Acc);
process_layout([Token | Tokens],
                  IsStartOfBlock,
                  IndentStack,
                  Acc) ->
	%% end and/or semicolon
	{Line, Col} = get_line_col(Token),
	{TokensBefore, IndentStack1} = tokens_before(Line, Col, IndentStack),
	%% begin and push indent to stack
	IsExplicitBegin = case Token of {'(', _} -> true; _ -> false end,
	{TokenBlockBegin, IndentStack2} =
		case IsStartOfBlock and not IsExplicitBegin of
			true  -> {[{'(', {Line, Col}}], [Col|IndentStack1]};
			false -> {[], IndentStack1}
		end,
	NextIsStartOfBlock = case Token of
		{Keyword, {_, _}} -> keyword_starts_offside_block(Keyword);
		_                 -> false
	end,
	Acc1 = [Token] ++ TokenBlockBegin ++ TokensBefore ++ Acc,
	process_layout(Tokens, NextIsStartOfBlock, IndentStack2, Acc1).

%% @doc Produces a list of extra tokens to insert before the next token,
%% starting on column Col
-spec tokens_before(_Line, _Col, _IndentStack) -> {[token()], _IndentStack}.
tokens_before(Line, Col, IndentStack=[Indent|Tail]) ->
	if
		Col < Indent ->
			{Tokens, IndentStack1} = tokens_before(Line, Col, Tail),
			{Tokens ++ [{')', {Line, Col}}], IndentStack1};
		Col == Indent ->
			{[{';', {Line, Col}}], IndentStack};
		Col > Indent ->
			{[], IndentStack}
	end.

keyword_starts_offside_block(X) when X =:= 'of'; X =:= '->';
                                     X =:= 'then'; X =:= 'else' ->
	true;
keyword_starts_offside_block(_) ->
	false.
