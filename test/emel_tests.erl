-module(emel_tests).
-include_lib("eunit/include/eunit.hrl").

% parser tests

check_tree(Expr, Expected) ->
    {ok, Tree, _} = emel:get_tree(Expr),
    ?assertEqual(Expected, Tree).

parser_simple_node_test() ->
    check_tree("html", [{node, 1, html, [], [], []}]).

parser_node_filter_test() ->
    check_tree("html|e", [{filter, 1, {node, 1, html, [], [], []}, [e]}]).

parser_node_filters_test() ->
    check_tree("html|e|html", [{filter, 1, {node, 1, html, [], [], []}, [e, html]}]).

parser_id_only_node_is_div_test() ->
    check_tree("#foo", [{node, 1, 'div', [{id, foo}], [], []}]).

parser_class_only_node_is_div_test() ->
    check_tree(".foo", [{node, 1, 'div', [{class, [foo]}], [], []}]).

parser_classes_only_node_is_div_test() ->
    check_tree(".foo.bar.baz", [{node, 1, 'div', [{class, [foo, bar, baz]}], [], []}]).

parser_classes_only_empty_attr_is_div_test() ->
    check_tree("[type=\"\"]", [{node, 1, 'div', [], [{type, ""}], []}]).

parser_classes_only_attr_name_is_div_test() ->
    check_tree("[type]", [{node, 1, 'div', [], [{type, ""}], []}]).

parser_classes_only_attr_name_and_complete_attr_is_div_test() ->
    check_tree("[type name=\"username\"]", [{node, 1, 'div', [], [{type, ""}, {name, "username"}], []}]).

parser_classes_only_attr_complete_and_attr_name_is_div_test() ->
    check_tree("[name=\"username\" type]", [{node, 1, 'div', [], [{name, "username"}, {type, ""}], []}]).

parser_classes_only_attr_is_div_test() ->
    check_tree("[type=\"input\"]", [{node, 1, 'div', [], [{type, "input"}], []}]).

parser_classes_only_attrs_is_div_test() ->
    check_tree("[type=\"input\" name=\"username\"]",
               [{node, 1, 'div', [], [{type, "input"}, {name, "username"}], []}]).

parser_id_and_class_only_node_is_div_test() ->
    check_tree("#asd.foo", [{node, 1, 'div', [{id, asd}, {class, [foo]}], [], []}]).

parser_id_and_classes_only_node_is_div_test() ->
    check_tree("#asd.foo.bar.baz", [{node, 1, 'div', [{id, asd}, {class, [foo, bar, baz]}], [], []}]).

parser_attrs_id_and_class_only_node_is_div_test() ->
    check_tree("[type=\"input\"]#asd.foo", [{node, 1, 'div', [{id, asd}, {class, [foo]}], [{type, "input"}], []}]).

parser_attrs_id_and_classes_only_node_is_div_test() ->
    check_tree("[type=\"input\"]#asd.foo.bar.baz", [{node, 1, 'div', [{id, asd}, {class, [foo, bar, baz]}], [{type, "input"}], []}]).

parser_tag_attrs_and_class_node_test() ->
    check_tree("span[type=\"input\"].foo", [{node, 1, span, [{class, [foo]}], [{type, "input"}], []}]).

parser_tag_attrs_and_classes_test() ->
    check_tree("span[type=\"input\"].foo.bar.baz", [{node, 1, span, [{class, [foo, bar, baz]}], [{type, "input"}], []}]).

parser_tag_attrs_and_id_node_test() ->
    check_tree("span[type=\"input\"]#asd", [{node, 1, span, [{id, asd}], [{type, "input"}], []}]).

parser_tag_attrs_id_and_class_node_test() ->
    check_tree("span[type=\"input\"]#asd.foo", [{node, 1, span, [{id, asd}, {class, [foo]}], [{type, "input"}], []}]).

parser_tag_attrs_id_and_classes_test() ->
    check_tree("span[type=\"input\"]#asd.foo.bar.baz", [{node, 1, span, [{id, asd}, {class, [foo, bar, baz]}], [{type, "input"}], []}]).

parser_tag_attrs_id_and_classes_times_test() ->
    check_tree("span[type=\"input\"]#asd.foo.bar.baz * 32", [
            {times, 1,
             {node, 1, span, [{id, asd}, {class, [foo, bar, baz]}], [{type, "input"}], []},
             32}]).

parser_tag_and_id_node_test() ->
    check_tree("span#asd", [{node, 1, span, [{id, asd}], [], []}]).

parser_tag_and_class_node_test() ->
    check_tree("span.foo", [{node, 1, span, [{class, [foo]}], [], []}]).

parser_tag_and_classes_test() ->
    check_tree("span.foo.bar.baz", [{node, 1, span, [{class, [foo, bar, baz]}], [], []}]).

parser_tag_id_and_class_node_test() ->
    check_tree("span#asd.foo", [{node, 1, span, [{id, asd}, {class, [foo]}], [], []}]).

parser_tag_id_and_classes_test() ->
    check_tree("span#asd.foo.bar.baz", [{node, 1, span, [{id, asd}, {class, [foo, bar, baz]}], [], []}]).

parser_siblings_2_nodes_test() ->
    check_tree("p+span", [{node, 1, p, [], [], []}, {node, 1, span, [], [], []}]).

parser_siblings_3_nodes_test() ->
    check_tree("p+span+#foo", [{node, 1, p, [], [], []}, {node, 1, span, [], [], []}, {node, 1, 'div', [{id, foo}], [], []}]).

parser_siblings_3_nodes_with_spaces_test() ->
    check_tree(" p +  span +   #foo  ", [{node, 1, p, [], [], []}, {node, 1, span, [], [], []}, {node, 1, 'div', [{id, foo}], [], []}]).

parser_node_times_test() ->
    check_tree("p * 3", [{times, 1, {node, 1, p, [], [], []}, 3}]).

parser_siblings_3_nodes_times_test() ->
    check_tree(" p  * 1 +  span*2 +   #foo* 3  ", [
            {times, 1, {node, 1, p, [], [], []}, 1},
            {times, 1, {node, 1, span, [], [], []}, 2},
            {times, 1, {node, 1, 'div', [{id, foo}], [], []}, 3}]).

parser_child_test() ->
    check_tree("li > a", [{child, 1,
                           {node, 1, li, [], [], []},
                           {node, 1, a, [], [], []}}]).

parser_child_times_test() ->
    check_tree("li * 3 > a", [{child, 1,
                               {times, 1, {node, 1, li, [], [], []}, 3},
                               {node, 1, a, [], [], []}}]).

parser_siblings_child_times_test() ->
    check_tree("p + li > a", [{node, 1, p, [], [], []},
                              {child, 1,
                               {node, 1, li, [], [], []},
                               {node, 1, a, [], [], []}}]).

parser_child_times_in_child_test() ->
    check_tree("li > a * 8", [{child, 1,
                               {node, 1, li, [], [], []},
                               {times, 1, {node, 1, a, [], [], []}, 8}}]).
parser_childs_test() ->
    check_tree("li > span > a", [
            {child, 1, {node, 1, li, [], [], []},
             {child, 1, {node, 1, span, [], [], []}, {node, 1, a, [], [], []}}}]).

parser_parent_test() ->
    check_tree("li < a", [{parent, 1,
                           {node, 1, li, [], [], []},
                           {node, 1, a, [], [], []}}]).

parser_parent_times_test() ->
    check_tree("li * 3 < a", [{parent, 1,
                               {times, 1, {node, 1, li, [], [], []}, 3},
                               {node, 1, a, [], [], []}}]).

parser_siblings_parent_times_test() ->
    check_tree("p + li < a", [{node, 1, p, [], [], []},
                              {parent, 1,
                               {node, 1, li, [], [], []},
                               {node, 1, a, [], [], []}}]).

parser_parent_times_in_parent_test() ->
    check_tree("li < a * 8", [{parent, 1,
                               {node, 1, li, [], [], []},
                               {times, 1, {node, 1, a, [], [], []}, 8}}]).
parser_parents_test() ->
    check_tree("li < span < a", [
            {parent, 1, {node, 1, li, [], [], []},
             {parent, 1, {node, 1, span, [], [], []}, {node, 1, a, [], [], []}}}]).

parser_child_and_parent_test() ->
    check_tree("li > span < a", [
            {child, 1, {node, 1, li, [], [], []},
             {parent, 1, {node, 1, span, [], [], []}, {node, 1, a, [], [], []}}}]).

parser_simple_parens_test() ->
    check_tree("(a)", [[{node, 1, a, [], [], []}]]).

parser_only_text_test() ->
    check_tree("{hello world}", [{text, 1, "hello world"}]).

parser_simple_text_test() ->
    check_tree("p{hello world}", [{node, 1, p, [], [], "hello world"}]).

parser_text_times_test() ->
    check_tree("p * 3 {hello world}", [{times, 1, {node, 1, p, [], [], "hello world"}, 3}]).

parser_text_complex_test() ->
    check_tree("p>({Click }+a{here}+{ to continue})",
               [{child, 1, {node, 1, p, [], [], []},
                [{text, 1, "Click "},
                {node, 1, a, [], [], "here"},
                 {text, 1, " to continue"}]}]).


parser_parens_test() ->
    check_tree("#page>(#header>ul#nav>li*4>a)+(#page>((h1>span)+p*2))+#footer",
               [
                {child, 1,
                 {node, 1, 'div', [{id, page}], [], []},
                 [
                  {child, 1,
                   {node, 1, 'div', [{id, header}], [], []},
                   {child, 1,
                    {node, 1, ul, [{id, nav}], [], []},
                    {child, 1,
                     {times, 1, {node, 1, li, [], [], []}, 4},
                     {node, 1, a, [], [], []}
                    }
                   }
                  }
                 ]
                },

                [
                 {child, 1,
                  {node, 1, 'div', [{id, page}], [], []},
                  [
                   [{child, 1,
                     {node, 1, h1, [], [], []},
                     {node, 1, span, [], [], []}
                   }],
                   {times, 1, {node, 1, p, [], [], []}, 2}
                  ]
                 }
                ],

                {node, 1, 'div', [{id, footer}], [], []}
               ]).

% lexer tests

check_lex(Expr, Expected) ->
    {ok, Result} = emel:get_tokens(Expr),
    ?assertEqual(Expected, Result).

check_single_token(Expr, Tag, Token) ->
    check_lex(Expr, [{Tag, 1, Token}]).

lex_identifier_test() ->
    check_single_token("html", identifier, html),
    check_single_token("$foo", identifier, '$foo'),
    check_single_token("foo-$", identifier, 'foo-$').

lex_string_test() ->
    check_single_token("\"\"", string, ""),
    check_single_token("\"\\\"\"", string, "\""),
    check_single_token("\"a\"", string, "a").

lex_number_test() ->
    check_single_token("0", number, 0),
    check_single_token("10", number, 10),
    check_single_token("01", number, 1),
    check_single_token("9", number, 9).

lex_id_test() ->
    check_single_token("#foo", id, foo),
    check_single_token("#foo-bar", id, 'foo-bar').

lex_class_test() ->
    check_single_token(".foo", cls, foo),
    check_single_token(".foo-bar", cls, 'foo-bar').

lex_tokens_test() ->
    check_single_token("+", sibling, '+'),
    check_single_token(">", child, '>'),
    check_single_token("<", parent, '<'),
    check_single_token("*", times, '*'),
    check_single_token("[", openattr, '['),
    check_single_token("]", closeattr, ']'),
    check_single_token("=", equal, '='),
    check_single_token("|", filter, '|').

% gen tests

check_gen(Expr, Expected) ->
    {ok, Text} = emel:gen(Expr),
    ?assertEqual(Expected, Text).

gen_simple_tag_test() ->
    check_gen("a", "<a/>").

gen_simple_times_test() ->
    check_gen("a * 3", "<a/><a/><a/>").

gen_times_test() ->
    check_gen("#foo + a * 3 + span", "<div id=\"foo\"/><a/><a/><a/><span/>").

gen_times_count_test() ->
    check_gen("a#foo-$ * 3", "<a id=\"foo-1\"/><a id=\"foo-2\"/><a id=\"foo-3\"/>").

%gen_hardcore_times_count_test() ->
%    check_gen("#out-$ * 2 > a#foo-$ * 2", "<div id=\"out-1\"><a id=\"foo-1\"/><a id=\"foo-2\"/></div><div id=\"out-2\"><a id=\"foo-1\"/><a id=\"foo-2\"/></div>").

gen_only_id_test() ->
    check_gen("#foo", "<div id=\"foo\"/>").

gen_only_class_test() ->
    check_gen(".foo", "<div class=\"foo\"/>").

gen_only_classes_test() ->
    check_gen(".foo.bar.baz", "<div class=\"foo bar baz\"/>").

gen_only_id_and_class_test() ->
    check_gen("#asd.foo", "<div id=\"asd\" class=\"foo\"/>").

gen_only_id_and_classes_test() ->
    check_gen("#asd.foo.bar.baz", "<div id=\"asd\" class=\"foo bar baz\"/>").

gen_only_attrs_id_and_classes_test() ->
    check_gen("[type=\"text\"]#asd.foo.bar.baz", "<div id=\"asd\" class=\"foo bar baz\" type=\"text\"/>").

gen_tag_attrs_id_and_classes_test() ->
    check_gen("span[type=\"text\"]#asd.foo.bar.baz", "<span id=\"asd\" class=\"foo bar baz\" type=\"text\"/>").

gen_sibling_test() ->
    check_gen("a + p + br", "<a/><p/><br/>").

gen_simple_child_test() ->
    check_gen("p > a", "<p><a/></p>").

gen_child_test() ->
    check_gen("p > #foo > a", "<p><div id=\"foo\"><a/></div></p>").

gen_child_1_test() ->
    check_gen("select>option#item-$*3", "<select><option id=\"item-1\"/><option id=\"item-2\"/><option id=\"item-3\"/></select>").

gen_parent_times_test() ->
    check_gen("p * 3 > a", "<p><a/></p><p><a/></p><p><a/></p>").

gen_parent_times_count_test() ->
    check_gen("p#id-$ * 3 > a", "<p id=\"id-1\"><a/></p><p id=\"id-2\"><a/></p><p id=\"id-3\"><a/></p>").

gen_parent_times_count_1_test() ->
    check_gen("ul>li#id-$ * 3 > a", "<ul><li id=\"id-1\"><a/></li><li id=\"id-2\"><a/></li><li id=\"id-3\"><a/></li></ul>").

gen_only_text_test() ->
    check_gen("{hello world}", "hello world").

gen_simple_text_test() ->
    check_gen("p{hello world}", "<p>hello world</p>").

gen_text_times_test() ->
    check_gen("p * 2 {hello world}", "<p>hello world</p><p>hello world</p>").

gen_text_complex_test() ->
    check_gen("p>({Click }+a{here}+{ to continue})", "<p>Click <a>here</a> to continue</p>").
