-module(escaper_tests).

-include_lib("eunit/include/eunit.hrl").

%% LIST TESTS
%% -----------------------------------------------------------------------------

ampersand_test() ->
	Str = "Chip & Dale",
	Expected = "Chip &amp; Dale",
	?assertEqual(Expected, escaper:escape(Str)).

brackets_test() ->
	Str = "Hello, <b>world</b>",
	Expected = "Hello, &lt;b&gt;world&lt;/b&gt;",
	?assertEqual(Expected, escaper:escape(Str)).

quotes_test() ->
	Str = "She says \"I love you\"",
	Expected = "She says &quot;I love you&quot;",
	?assertEqual(Expected, escaper:escape(Str)).

single_quote_test() ->
	Str = "I'll be back",
	Expected = "I&#x27;ll be back",
	?assertEqual(Expected, escaper:escape(Str)).

backticks_test() ->
	Str = "`backticks`",
	Expected = "&#x60;backticks&#x60;",
	?assertEqual(Expected, escaper:escape(Str)).

utf8_test() ->
	Str = "Ромео & Джульетта",
	Expected = "Ромео &amp; Джульетта",
	?assertEqual(Expected, escaper:escape(Str)).

%% BINARY TESTS
%% -----------------------------------------------------------------------------

ampersand_bin_test() ->
	Str = <<"Chip & Dale">>,
	Expected = <<"Chip &amp; Dale">>,
	?assertEqual(Expected, escaper:escape(Str)).

brackets_bin_test() ->
	Str = <<"Hello, <b>world</b>">>,
	Expected = <<"Hello, &lt;b&gt;world&lt;/b&gt;">>,
	?assertEqual(Expected, escaper:escape(Str)).

quotes_bin_test() ->
	Str = <<"She says \"I love you\"">>,
	Expected = <<"She says &quot;I love you&quot;">>,
	?assertEqual(Expected, escaper:escape(Str)).

single_bin_quote_test() ->
	Str = <<"I'll be back">>,
	Expected = <<"I&#x27;ll be back">>,
	?assertEqual(Expected, escaper:escape(Str)).

backticks_bin_test() ->
	Str = <<"`backticks`">>,
	Expected = <<"&#x60;backticks&#x60;">>,
	?assertEqual(Expected, escaper:escape(Str)).

utf8_bin_test() ->
	Str = <<"Ромео & Джульетта"/utf8>>,
	Expected = <<"Ромео &amp; Джульетта"/utf8>>,
	?assertEqual(Expected, escaper:escape(Str)).
