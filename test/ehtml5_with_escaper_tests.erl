-module(ehtml5_with_escaper_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TO_HTML(EHTML5), unicode:characters_to_binary(ehtml5:render(EHTML5))).

escape_tag_with_content_test() ->
	EHTML5 = {p, "Chip & Dale"},
	Expected = <<"<p>Chip &amp; Dale</p>">>,
	?assertEqual(Expected, ?TO_HTML(EHTML5)).

escape_tag_with_bin_content_test() ->
	EHTML5 = {p, <<"Chip & Dale">>},
	Expected = <<"<p>Chip &amp; Dale</p>">>,
	?assertEqual(Expected, ?TO_HTML(EHTML5)).

escape_tag_with_attr_and_content_test() ->
	EHTML5 = {span, #{id => <<"name">>}, "<script>alert()</script>"},
	Expected = <<"<span id=\"name\">&lt;script&gt;alert()&lt;/script&gt;</span>">>,
	?assertEqual(Expected, ?TO_HTML(EHTML5)).

escape_tag_with_attr_and_bin_content_test() ->
	EHTML5 = {p, #{id => <<"xss">>}, <<"\"><script>alert()</script>">>},
	Expected = <<"<p id=\"xss\">&quot;&gt;&lt;script&gt;alert()&lt;/script&gt;</p>">>,
	?assertEqual(Expected, ?TO_HTML(EHTML5)).

escape_attr_test() ->
	EHTML5 = [a, #{href => {"/search?k1=v1&k2=v2"}}, "search"],
	Expected = <<"<a href=\"/search?k1=v1&amp;k2=v2\">search</a>">>,
	?assertEqual(Expected, ?TO_HTML(EHTML5)).

escape_bin_attr_test() ->
	EHTML5 = [em, #{'data-name' => {<<"е`жик"/utf8>>}}],
	Expected = <<"<em data-name=\"е&#x60;жик\"></em>"/utf8>>,
	?assertEqual(Expected, ?TO_HTML(EHTML5)).
