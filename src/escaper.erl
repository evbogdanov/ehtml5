%% HTML ENTITIES FOR ESCAPING
%% -----------------------------------------------------------------------------
%%
%% $& => "&amp;",
%% $< => "&lt;",
%% $> => "&gt;",
%% $" => "&quot;",
%% $' => "&#x27;",
%% $` => "&#x60;"
%%

-module(escaper).

-export([
	escape/1
]).

%% COMMON API FOR LIST / BINARY
%% -----------------------------------------------------------------------------

escape(String) when is_list(String) ->
	escape_str(String, "");

escape(String) when is_binary(String) ->
	escape_bin(String, <<>>).

%% IF STRING IS LIST
%% -----------------------------------------------------------------------------

escape_str("", Acc) ->
	lists:reverse(Acc);

escape_str([$& | T], Acc) ->
	escape_str(T, [$;, $p, $m, $a, $& | Acc]);

escape_str([$< | T], Acc) ->
	escape_str(T, [$;, $t, $l, $& | Acc]);

escape_str([$> | T], Acc) ->
	escape_str(T, [$;, $t, $g, $& | Acc]);

escape_str([$" | T], Acc) ->
	escape_str(T, [$;, $t, $o, $u, $q, $& | Acc]);

escape_str([$' | T], Acc) ->
	escape_str(T, [$;, $7, $2, $x, $#, $& | Acc]);

escape_str([$` | T], Acc) ->
	escape_str(T, [$;, $0, $6, $x, $#, $& | Acc]);

escape_str([H | T], Acc) ->
	escape_str(T, [H | Acc]).

%% IF STRING IS BINARY
%% -----------------------------------------------------------------------------

escape_bin(<<>>, Acc) ->
	Acc;

escape_bin(<<$&, Rest/binary>>, Acc) ->
	Esc = <<"&amp;">>,
	escape_bin(Rest, <<Acc/binary, Esc/binary>>);

escape_bin(<<$<, Rest/binary>>, Acc) ->
	Esc = <<"&lt;">>,
	escape_bin(Rest, <<Acc/binary, Esc/binary>>);

escape_bin(<<$>, Rest/binary>>, Acc) ->
	Esc = <<"&gt;">>,
	escape_bin(Rest, <<Acc/binary, Esc/binary>>);

escape_bin(<<$", Rest/binary>>, Acc) ->
	Esc = <<"&quot;">>,
	escape_bin(Rest, <<Acc/binary, Esc/binary>>);

escape_bin(<<$', Rest/binary>>, Acc) ->
	Esc = <<"&#x27;">>,
	escape_bin(Rest, <<Acc/binary, Esc/binary>>);

escape_bin(<<$`, Rest/binary>>, Acc) ->
	Esc = <<"&#x60;">>,
	escape_bin(Rest, <<Acc/binary, Esc/binary>>);

escape_bin(<<H, Rest/binary>>, Acc) ->
	escape_bin(Rest, <<Acc/binary, H>>).
