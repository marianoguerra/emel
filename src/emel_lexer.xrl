Definitions.

Identifier = [a-zA-Z\$][a-zA-Z0-9\-\$]*
Number     = [0-9][0-9]*
String     = "(\\\^.|\\.|[^\"])*"
Text       = \{(\\.|[^\}])*\}

IdPrefix   = #
ClsPrefix  = \.
Child      = >
Parent     = <
Sibling    = \+
Times      = \*
OpenAttr   = \[
CloseAttr  = \]
Open       = \(
Close      = \)
Equal      = =
Filter     = \|
Whites     = \s+

Rules.

{Identifier}             : make_token(identifier, TokenLine, TokenChars).
{String}                 : build_string(string, TokenChars, TokenLine, TokenLen).
{Number}                 : make_token(number, TokenLine, TokenChars, fun erlang:list_to_integer/1).
{Text}                   : build_text(text, TokenChars, TokenLine, TokenLen).

{IdPrefix}{Identifier}   : make_token(id, TokenLine, tl(TokenChars)).
{ClsPrefix}{Identifier}  : make_token(cls, TokenLine, tl(TokenChars)).

{Child}                  : make_token(child, TokenLine, TokenChars).
{Parent}                 : make_token(parent, TokenLine, TokenChars).
{Sibling}                : make_token(sibling, TokenLine, TokenChars).
{Times}                  : make_token(times, TokenLine, TokenChars).
{OpenAttr}               : make_token(openattr, TokenLine, TokenChars).
{CloseAttr}              : make_token(closeattr, TokenLine, TokenChars).
{Open}                   : make_token(open, TokenLine, TokenChars).
{Close}                  : make_token(close, TokenLine, TokenChars).
{Equal}                  : make_token(equal, TokenLine, TokenChars).
{Filter}                 : make_token(filter, TokenLine, TokenChars).
{Whites}                 : skip_token.

Erlang code.

make_token(Name, Line, Chars) when is_list(Chars) ->
    {token, {Name, Line, list_to_atom(Chars)}};
make_token(Name, Line, Chars) ->
    {token, {Name, Line, Chars}}.

make_token(Name, Line, Chars, Fun) ->
    {token, {Name, Line, Fun(Chars)}}.

build_string(Type, Chars, Line, Len) ->
  String = unescape_string(lists:sublist(Chars, 2, Len - 2), Line),
    {token, {Type, Line, String}}.

build_text(Type, Chars, Line, Len) ->
  Text = lists:sublist(Chars, 2, Len - 2),
    {token, {Type, Line, Text}}.

unescape_string(String, Line) -> unescape_string(String, Line, []).

unescape_string([], _Line, Output) ->
  lists:reverse(Output);
unescape_string([$\\, Escaped | Rest], Line, Output) ->
  Char = map_escaped_char(Escaped, Line),
  unescape_string(Rest, Line, [Char|Output]);
unescape_string([Char|Rest], Line, Output) ->
  unescape_string(Rest, Line, [Char|Output]).

map_escaped_char(Escaped, Line) ->
  case Escaped of
    $\\ -> $\\;
    $/ -> $/;
    $\" -> $\";
    $\' -> $\';
    $\( -> $(;
    $b -> $\b;
    $d -> $\d;
    $e -> $\e;
    $f -> $\f;
    $n -> $\n;
    $r -> $\r;
    $s -> $\s;
    $t -> $\t;
    $v -> $\v;
    _ -> throw({error, {Line, fn_lexer, ["unrecognized escape sequence: ", [$\\, Escaped]]}})
  end.
