-module(emel).

-export([gen/1, shell/0]).

-ifdef(TEST).
-compile(export_all).
-endif.

get_tokens(String) ->
    case emel_lexer:string(String) of
        {ok, Tokens, _EndLine} ->
            {ok, Tokens};
        Errors ->
            {false, Errors}
    end.

get_tree(String) ->
    {ok, Tokens} = get_tokens(String),

    case emel_parser:parse(Tokens) of
        {ok, Tree}                        -> {ok, Tree, []};
        {ok, _Tree, _}=Result             -> Result;
        {error, _Warnings, Errors}=Errors -> Errors
    end.

replace_count_placeholder(Attrs, none) ->
    Attrs;

replace_count_placeholder(Attrs, Count) ->
    replace_count_placeholder(Attrs, integer_to_list(Count), []).

replace_count_placeholder([], _Count, Accum) ->
    lists:reverse(Accum);

replace_count_placeholder([Attr|Attrs], Count, Accum) ->
    {AttrName, AttrValue} = Attr,
    ReplacedAttr = {AttrName, re:replace(AttrValue, "\\$", Count, [global])},
    replace_count_placeholder(Attrs, Count, [ReplacedAttr|Accum]).

process_iattrs(IAttrs, Count) ->
    process_iattrs(IAttrs, Count, []).

process_iattrs([], Count, Accum) ->
    replace_count_placeholder(lists:reverse(Accum), Count);

process_iattrs([{Name, Val}|T], Count, Accum) when is_list(Val) ->
    Vals = string:join([atom_to_list(V) || V <- Val], " "),
    process_iattrs(T, Count, [{Name, Vals}|Accum]);

process_iattrs([{Name, Val}|T], Count, Accum) ->
    process_iattrs(T, Count, [{Name, atom_to_list(Val)}|Accum]).

join_attrs(IAttrs, Attrs, Count) ->
    process_iattrs(IAttrs, Count) ++ replace_count_placeholder(Attrs, Count).

process_node({node, _Line, Name, IAttrs, Attrs}, Accum, Count) ->
    [{Name, join_attrs(IAttrs, Attrs, Count), []}|Accum];

process_node({child, _Line, {node, _Line1, Name, IAttrs, Attrs}, Child}, Accum, Count) ->
    [{Name, join_attrs(IAttrs, Attrs, Count), process_tree(Child)}|Accum];

process_node({times, _Line, _Node, 0}, Accum, _) ->
    Accum;

process_node({times, _Line, _Node, _Times}=TimesNode, Accum, ParentTimes) ->
    process_node(TimesNode, Accum, ParentTimes, 0).

% the value of Times in the expression and in the counter (last parameter)
% are equal
process_node({times, _Line, _Node, Times}, Accum, _ParentTimes, Times) ->
    Accum;

process_node({times=Name, Line, Node, Times}, Accum, ParentTimes, Count) ->
    process_node({Name, Line, Node, Times}, process_node(Node, Accum, Count + 1), ParentTimes, Count + 1).

process_tree(Tree) when is_tuple(Tree) ->
    process_tree([Tree]);

process_tree(Tree) ->
    process_tree(Tree, []).

process_tree([], Accum) ->
    lists:reverse(Accum);

process_tree([H|T], Accum) ->
    process_tree(T, process_node(H, Accum, none)).

gen(Expr) ->
    {ok, Tree, _} = get_tree(Expr),
    PTree = process_tree(Tree),

    XmlIOList = xmerl:export_simple_content(PTree, xmerl_xml),
    XmlString = lists:flatten(XmlIOList),

    {ok, XmlString}.

shell_loop() ->
    Expression = io:get_line(">>> "),
    if
        Expression /= eof ->
            Expr = string:strip(Expression, right, $\n),
            io:format("~p~n~n", [get_tree(Expr)]),
            io:format("~s~n~n", [element(2, gen(Expr))]),
            shell_loop();

        true ->
            io:format("bye~n")
    end.

shell() ->
    shell_loop().
