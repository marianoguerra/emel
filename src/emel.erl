-module(emel).

-export([html/1]).

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

html(Expr) ->
    {ok, Tree, _} = get_tree(Expr),

    Tree.
