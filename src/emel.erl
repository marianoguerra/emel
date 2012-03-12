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

process_iattrs(IAttrs) ->
    process_iattrs(IAttrs, []).

process_iattrs([], Accum) ->
    lists:reverse(Accum);

process_iattrs([{Name, Val}|T], Accum) when is_list(Val) ->
    Vals = string:join([atom_to_list(V) || V <- Val], " "),
    process_iattrs(T, [{Name, Vals}|Accum]);

process_iattrs([{Name, Val}|T], Accum) ->
    process_iattrs(T, [{Name, atom_to_list(Val)}|Accum]).

join_attrs(IAttrs, Attrs) ->
    process_iattrs(IAttrs) ++ Attrs.

process_node({node, _Line, Name, IAttrs, Attrs}) ->
    {Name, join_attrs(IAttrs, Attrs), []}.

process_tree(Tree) ->
    process_tree(Tree, []).

process_tree([], Accum) ->
    lists:reverse(Accum);

process_tree([H|T], Accum) ->
    process_tree(T, [process_node(H)|Accum]).

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
