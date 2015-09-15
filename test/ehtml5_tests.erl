-module(ehtml5_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TO_HTML(EHTML5), unicode:characters_to_binary(ehtml5:render(EHTML5))).

doctype_test() ->
	EHTML5 = [html],
	?assertEqual(<<"<!DOCTYPE html><html></html>">>, ?TO_HTML(EHTML5)).

tag_test() ->
	EHTML5 = [p],
	?assertEqual(<<"<p></p>">>, ?TO_HTML(EHTML5)).

self_closing_tag_test() ->
	EHTML5 = [br],
	?assertEqual(<<"<br>">>, ?TO_HTML(EHTML5)).

tag_with_content_test() ->
	EHTML5 = [p, <<"paragraph">>],
	?assertEqual(<<"<p>paragraph</p>">>, ?TO_HTML(EHTML5)).

tag_with_list_content_test() ->
	EHTML5 = [p, "paragraph"],
	?assertEqual(<<"<p>paragraph</p>">>, ?TO_HTML(EHTML5)).

tag_with_unicode_content_test() ->
	EHTML5 = [p, <<"параграф"/utf8>>],
	?assertEqual(<<"<p>параграф</p>"/utf8>>, ?TO_HTML(EHTML5)).

tag_with_unicode_list_content_test() ->
	EHTML5 = [p, "параграф"],
	?assertEqual(<<"<p>параграф</p>"/utf8>>, ?TO_HTML(EHTML5)).

self_closing_tag_with_content_test() -> % bad?
	EHTML5 = [img, <<" alt">>],
	?assertEqual(<<"<img alt>">>, ?TO_HTML(EHTML5)).

tag_with_attr_test() ->
	EHTML5 = ['div', #{id => <<"ID">>}],
	?assertEqual(<<"<div id=\"ID\"></div>">>, ?TO_HTML(EHTML5)).

html_tag_with_attr_test() ->
	EHTML5 = [html, #{lang => <<"en">>}],
	?assertEqual(<<"<!DOCTYPE html><html lang=\"en\"></html>">>, ?TO_HTML(EHTML5)).

tag_with_attr_and_content_test() ->
	EHTML5 = ['div', #{id => <<"ID">>}, <<"CONTENT">>],
	?assertEqual(<<"<div id=\"ID\">CONTENT</div>">>, ?TO_HTML(EHTML5)).

tag_with_two_attrs_and_content_test() ->
	EHTML5 = ['div', #{class => <<"CLASS">>, id => <<"ID">>}, <<"CONTENT">>],
	?assertEqual(<<"<div class=\"CLASS\" id=\"ID\">CONTENT</div>">>, ?TO_HTML(EHTML5)).

tag_with_attr_and_list_content_test() ->
	EHTML5 = [a, #{href => "/test"}, "test"],
	?assertEqual(<<"<a href=\"/test\">test</a>">>, ?TO_HTML(EHTML5)).

child_test() ->
	EHTML5 = ['div', [span]],
	?assertEqual(<<"<div><span></span></div>">>, ?TO_HTML(EHTML5)).

self_closing_child_test() ->
	EHTML5 = [form, [input]],
	?assertEqual(<<"<form><input></form>">>, ?TO_HTML(EHTML5)).

children_test() ->
	EHTML5 = [ul, [
		[li],
		[li]
	]],
	?assertEqual(<<"<ul><li></li><li></li></ul>">>, ?TO_HTML(EHTML5)).

self_closing_children_test() ->
	EHTML5 = [form, [
		[input],
		[input]
	]],
	?assertEqual(<<"<form><input><input></form>">>, ?TO_HTML(EHTML5)).

siblings_test() ->
	EHTML5 = [
		[h1],
		[p]
	],
	?assertEqual(<<"<h1></h1><p></p>">>, ?TO_HTML(EHTML5)).

self_closing_siblings_test() ->
	EHTML5 = [
		[h1],
		[br],
		[p]
	],
	?assertEqual(<<"<h1></h1><br><p></p>">>, ?TO_HTML(EHTML5)).

children_with_attr_test() ->
	EHTML5 = [ul, #{id => <<"nav">>}, [
		[li, #{class => <<"first">>}],
		[li, #{class => <<"last">>}]
	]],
	?assertEqual(<<"<ul id=\"nav\"><li class=\"first\"></li><li class=\"last\"></li></ul>">>, ?TO_HTML(EHTML5)).

data_attrs_test() ->
	EHTML5 = [p, #{'data-test' => <<"test">>}],
	?assertEqual(<<"<p data-test=\"test\"></p>">>, ?TO_HTML(EHTML5)).

list_comprehension_test() ->
	List = [<<"uno">>, <<"dos">>],
	EHTML5 = [ul, [
		[li, Content] || Content <- List
	]],
	?assertEqual(<<"<ul><li>uno</li><li>dos</li></ul>">>, ?TO_HTML(EHTML5)).

case_of_test() ->
	EHTML5 = [p, case true of
		true  -> <<"ok">>;
		false -> <<"not ok">>
	end],
	?assertEqual(<<"<p>ok</p>">>, ?TO_HTML(EHTML5)).

nesting_test() ->
	EHTML5 = [ul, [
		[li, <<"a">>],
		[li, <<"b">>],
		[li, [
			[ul, [
				[li, <<"c">>],
				[li, <<"d">>]
			]]
		]],
		[li, <<"e">>]
	]],
	?assertEqual(<<"<ul><li>a</li><li>b</li><li><ul><li>c</li><li>d</li></ul></li><li>e</li></ul>">>, ?TO_HTML(EHTML5)).

nesting2_test() ->
	EHTML5 = [ul, [
		[li, <<"a">>],
		[li, <<"b">>],
		[li, [ul, [ % shorter form
			[li, <<"c">>],
			[li, <<"d">>]
		]]],
		[li, <<"e">>]
	]],
	?assertEqual(<<"<ul><li>a</li><li>b</li><li><ul><li>c</li><li>d</li></ul></li><li>e</li></ul>">>, ?TO_HTML(EHTML5)).

%% CONTENT TEST
%% -----------------------------------------------------------------------------

content_test() ->
	EHTML5 = <<"test">>,
	?assertEqual(<<"test">>, ?TO_HTML(EHTML5)).

list_content_test() ->
	EHTML5 = "test",
	?assertEqual(<<"test">>, ?TO_HTML(EHTML5)).

unicode_list_content_test() ->
	EHTML5 = "тест",
	?assertEqual(<<"тест"/utf8>>, ?TO_HTML(EHTML5)).

combo_content_test() ->
	EHTML5 = [<<"a">>, "b", <<"в"/utf8>>],
	?assertEqual(<<"abв"/utf8>>, ?TO_HTML(EHTML5)).

combo_content2_test() ->
	EHTML5 = [footer, [<<"2015">>, " © footer"]],
	?assertEqual(<<"<footer>2015 © footer</footer>"/utf8>>, ?TO_HTML(EHTML5)).

%% MEGA TEST
%% -----------------------------------------------------------------------------

mega_test() ->
	EHTML5 = [html, #{lang => <<"en">>}, [
		[head, [
			[meta, #{charset => <<"UTF-8">>}],
			[title, <<"EHTML5">>],
			[script, #{type => <<"text/javascript">>}, <<"if (foo) bar(1 + 2);">>]
		]],
		[body, [
			[h1, <<"EHTML5">>],
			['div', #{id => <<"container">>, class => <<"col">>}, [
				[p, <<"What is EHTML5?">>],
				[p, <<"EHTML5 is a simple template engine inspired by Yaws's EHTML.">>]
			]]
		]]
	]],
	Expected = <<"<!DOCTYPE html><html lang=\"en\"><head><meta charset=\"UTF-8\"><title>EHTML5</title><script type=\"text/javascript\">if (foo) bar(1 + 2);</script></head><body><h1>EHTML5</h1><div class=\"col\" id=\"container\"><p>What is EHTML5?</p><p>EHTML5 is a simple template engine inspired by Yaws's EHTML.</p></div></body></html>">>,
	?assertEqual(Expected, ?TO_HTML(EHTML5)).

mega_list_test() ->
	EHTML5 = [html, #{lang => "en"}, [
		[head, [
			[meta, #{charset => "UTF-8"}],
			[title, "EHTML5"],
			[script, #{type => "text/javascript"}, "if (foo) bar(1 + 2);"]
		]],
		[body, [
			[h1, "EHTML5"],
			['div', #{id => "container", class => "col"}, [
				[p, "What is EHTML5?"],
				[p, "EHTML5 is a simple template engine inspired by Yaws's EHTML."]
			]]
		]]
	]],
	Expected = <<"<!DOCTYPE html><html lang=\"en\"><head><meta charset=\"UTF-8\"><title>EHTML5</title><script type=\"text/javascript\">if (foo) bar(1 + 2);</script></head><body><h1>EHTML5</h1><div class=\"col\" id=\"container\"><p>What is EHTML5?</p><p>EHTML5 is a simple template engine inspired by Yaws's EHTML.</p></div></body></html>">>,
	?assertEqual(Expected, ?TO_HTML(EHTML5)).
