-module(emel_tests).
-include_lib("eunit/include/eunit.hrl").

% parser tests

check_tree(Expr, Expected) ->
    {ok, Tree, _} = emel:get_tree(Expr),
    ?assertEqual(Expected, Tree).

parser_simple_node_test() ->
    check_tree("html", [{node, 1, html, [], []}]).

parser_node_filter_test() ->
    check_tree("html|e", [{filter, 1, {node, 1, html, [], []}, [e]}]).

parser_node_filters_test() ->
    check_tree("html|e|html", [{filter, 1, {node, 1, html, [], []}, [e, html]}]).

parser_id_only_node_is_div_test() ->
    check_tree("#foo", [{node, 1, 'div', [{id, foo}], []}]).

parser_class_only_node_is_div_test() ->
    check_tree(".foo", [{node, 1, 'div', [{class, [foo]}], []}]).

parser_classes_only_node_is_div_test() ->
    check_tree(".foo.bar.baz", [{node, 1, 'div', [{class, [foo, bar, baz]}], []}]).

parser_classes_only_empty_attr_is_div_test() ->
    check_tree("[type=\"\"]", [{node, 1, 'div', [], [{type, ""}]}]).

parser_classes_only_attr_name_is_div_test() ->
    check_tree("[type]", [{node, 1, 'div', [], [{type, ""}]}]).

parser_classes_only_attr_name_and_complete_attr_is_div_test() ->
    check_tree("[type name=\"username\"]", [{node, 1, 'div', [], [{type, ""}, {name, "username"}]}]).

parser_classes_only_attr_complete_and_attr_name_is_div_test() ->
    check_tree("[name=\"username\" type]", [{node, 1, 'div', [], [{name, "username"}, {type, ""}]}]).

parser_classes_only_attr_is_div_test() ->
    check_tree("[type=\"input\"]", [{node, 1, 'div', [], [{type, "input"}]}]).

parser_classes_only_attrs_is_div_test() ->
    check_tree("[type=\"input\" name=\"username\"]",
               [{node, 1, 'div', [], [{type, "input"}, {name, "username"}]}]).

parser_id_and_class_only_node_is_div_test() ->
    check_tree("#asd.foo", [{node, 1, 'div', [{id, asd}, {class, [foo]}], []}]).

parser_id_and_classes_only_node_is_div_test() ->
    check_tree("#asd.foo.bar.baz", [{node, 1, 'div', [{id, asd}, {class, [foo, bar, baz]}], []}]).

parser_attrs_id_and_class_only_node_is_div_test() ->
    check_tree("[type=\"input\"]#asd.foo", [{node, 1, 'div', [{id, asd}, {class, [foo]}], [{type, "input"}]}]).

parser_attrs_id_and_classes_only_node_is_div_test() ->
    check_tree("[type=\"input\"]#asd.foo.bar.baz", [{node, 1, 'div', [{id, asd}, {class, [foo, bar, baz]}], [{type, "input"}]}]).

parser_tag_attrs_and_class_node_test() ->
    check_tree("span[type=\"input\"].foo", [{node, 1, span, [{class, [foo]}], [{type, "input"}]}]).

parser_tag_attrs_and_classes_test() ->
    check_tree("span[type=\"input\"].foo.bar.baz", [{node, 1, span, [{class, [foo, bar, baz]}], [{type, "input"}]}]).

parser_tag_attrs_and_id_node_test() ->
    check_tree("span[type=\"input\"]#asd", [{node, 1, span, [{id, asd}], [{type, "input"}]}]).

parser_tag_attrs_id_and_class_node_test() ->
    check_tree("span[type=\"input\"]#asd.foo", [{node, 1, span, [{id, asd}, {class, [foo]}], [{type, "input"}]}]).

parser_tag_attrs_id_and_classes_test() ->
    check_tree("span[type=\"input\"]#asd.foo.bar.baz", [{node, 1, span, [{id, asd}, {class, [foo, bar, baz]}], [{type, "input"}]}]).

parser_tag_attrs_id_and_classes_times_test() ->
    check_tree("span[type=\"input\"]#asd.foo.bar.baz * 32", [
            {times, 1,
             {node, 1, span, [
                        {id, asd},
                        {class, [foo, bar, baz]}],
                         [{type, "input"}]},
             32}]).

parser_tag_and_id_node_test() ->
    check_tree("span#asd", [{node, 1, span, [{id, asd}], []}]).

parser_tag_and_class_node_test() ->
    check_tree("span.foo", [{node, 1, span, [{class, [foo]}], []}]).

parser_tag_and_classes_test() ->
    check_tree("span.foo.bar.baz", [{node, 1, span, [{class, [foo, bar, baz]}], []}]).

parser_tag_id_and_class_node_test() ->
    check_tree("span#asd.foo", [{node, 1, span, [{id, asd}, {class, [foo]}], []}]).

parser_tag_id_and_classes_test() ->
    check_tree("span#asd.foo.bar.baz", [{node, 1, span, [{id, asd}, {class, [foo, bar, baz]}], []}]).

parser_siblings_2_nodes_test() ->
    check_tree("p+span", [{node, 1, p, [], []}, {node, 1, span, [], []}]).

parser_siblings_3_nodes_test() ->
    check_tree("p+span+#foo", [{node, 1, p, [], []}, {node, 1, span, [], []}, {node, 1, 'div', [{id, foo}], []}]).

parser_siblings_3_nodes_with_spaces_test() ->
    check_tree(" p +  span +   #foo  ", [{node, 1, p, [], []}, {node, 1, span, [], []}, {node, 1, 'div', [{id, foo}], []}]).

parser_node_times_test() ->
    check_tree("p * 3", [{times, 1, {node, 1, p, [], []}, 3}]).

parser_siblings_3_nodes_times_test() ->
    check_tree(" p  * 1 +  span*2 +   #foo* 3  ", [
            {times, 1, {node, 1, p, [], []}, 1},
            {times, 1, {node, 1, span, [], []}, 2},
            {times, 1, {node, 1, 'div', [{id, foo}], []}, 3}]).

parser_child_test() ->
    check_tree("li > a", [{child, 1,
                           {node, 1, li, [], []},
                           {node, 1, a, [], []}}]).

parser_child_times_test() ->
    check_tree("li * 3 > a", [{child, 1,
                           {times, 1, {node, 1, li, [], []}, 3},
                           {node, 1, a, [], []}}]).

parser_siblings_child_times_test() ->
    check_tree("p + li > a", [{node, 1, p, [], []},
                              {child, 1,
                               {node, 1, li, [], []},
                               {node, 1, a, [], []}}]).

parser_child_times_in_child_test() ->
    check_tree("li > a * 8", [{child, 1,
                           {node, 1, li, [], []},
                           {times, 1, {node, 1, a, [], []}, 8}}]).
parser_childs_test() ->
    check_tree("li > span > a", [
            {child, 1, {node, 1, li, [], []},
             {child, 1, {node, 1, span, [], []}, {node, 1, a, [], []}}}]).

parser_parent_test() ->
    check_tree("li < a", [{parent, 1,
                           {node, 1, li, [], []},
                           {node, 1, a, [], []}}]).

parser_parent_times_test() ->
    check_tree("li * 3 < a", [{parent, 1,
                           {times, 1, {node, 1, li, [], []}, 3},
                           {node, 1, a, [], []}}]).

parser_siblings_parent_times_test() ->
    check_tree("p + li < a", [{node, 1, p, [], []},
                              {parent, 1,
                               {node, 1, li, [], []},
                               {node, 1, a, [], []}}]).

parser_parent_times_in_parent_test() ->
    check_tree("li < a * 8", [{parent, 1,
                           {node, 1, li, [], []},
                           {times, 1, {node, 1, a, [], []}, 8}}]).
parser_parents_test() ->
    check_tree("li < span < a", [
            {parent, 1, {node, 1, li, [], []},
             {parent, 1, {node, 1, span, [], []}, {node, 1, a, [], []}}}]).

parser_child_and_parent_test() ->
    check_tree("li > span < a", [
            {child, 1, {node, 1, li, [], []},
             {parent, 1, {node, 1, span, [], []}, {node, 1, a, [], []}}}]).

parser_simple_parens_test() ->
    check_tree("(a)", [[{node, 1, a, [], []}]]).

parser_parens_test() ->
    check_tree("#page>(#header>ul#nav>li*4>a)+(#page>((h1>span)+p*2))+#footer",
               [
                {child, 1,
                 {node, 1, 'div', [{id, page}], []},
                 [
                  {child, 1,
                   {node, 1, 'div', [{id, header}], []},
                   {child, 1,
                    {node, 1, ul, [{id, nav}], []},
                    {child, 1,
                     {times, 1, {node, 1, li, [], []}, 4},
                     {node, 1, a, [], []}
                    }
                   }
                  }
                 ]
                },

                [
                 {child, 1,
                  {node, 1, 'div', [{id, page}], []},
                  [
                   [{child, 1,
                    {node, 1, h1, [], []},
                    {node, 1, span, [], []}
                   }],
                   {times, 1, {node, 1, p, [], []}, 2}
                  ]
                 }
                ],

                {node, 1, 'div', [{id, footer}], []}
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
