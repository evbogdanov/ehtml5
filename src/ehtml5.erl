-module(ehtml5).

-export([
  render/1
]).

-define(SELF_CLOSING_TAGS, [
  area,
  base,
  br,
  col,
  command,
  embed,
  hr,
  img,
  input,
  keygen,
  link,
  meta,
  param,
  source,
  track,
  wbr
]).

%%%=============================================================================
%%% w/o children
%%%=============================================================================

render([Tag]) when is_atom(Tag) ->
  [tag_opening(Tag), tag_middle(Tag), tag_closing(Tag)];

render([Tag, Content]) when is_atom(Tag), is_binary(Content) ->
  [tag_opening(Tag), tag_middle(Tag), Content, tag_closing(Tag)];

render([Tag, Attrs]) when is_atom(Tag), is_map(Attrs) ->
  [tag_opening(Tag), attrs(Attrs), tag_middle(Tag), tag_closing(Tag)];

render([Tag, Attrs, Content]) when is_atom(Tag), is_map(Attrs), is_binary(Content) ->
  [tag_opening(Tag), attrs(Attrs), tag_middle(Tag), Content, tag_closing(Tag)];

%%%=============================================================================
%%% w/ children
%%%=============================================================================

render([Tag, Children]) when is_atom(Tag), is_list(Children) ->
  [tag_opening(Tag), tag_middle(Tag), render(Children), tag_closing(Tag)];

render([Tag, Attrs, Children]) when is_atom(Tag), is_map(Attrs), is_list(Children) ->
  [tag_opening(Tag), attrs(Attrs), tag_middle(Tag), render(Children), tag_closing(Tag)];

render([FirstChild | OtherChildren]) ->
  [render(FirstChild) | render(OtherChildren)];

render([]) ->
  [].

%%%=============================================================================
%%% tags
%%%=============================================================================

tag_opening(html) ->
  "<!DOCTYPE html><html";

tag_opening(Tag) ->
  "<" ++ atom_to_list(Tag).

tag_middle(Tag) ->
  case lists:member(Tag, ?SELF_CLOSING_TAGS) of
    true  -> [];
    false -> ">"
  end.

tag_closing(Tag) ->
  case lists:member(Tag, ?SELF_CLOSING_TAGS) of
    true  -> ">";
    false -> "</" ++ atom_to_list(Tag) ++ ">"
  end.

%%%=============================================================================
%%% attrs
%%%=============================================================================

attrs(Attrs) when is_map(Attrs) ->
  [[" ", atom_to_list(A), "=\"", maps:get(A, Attrs), "\""] || A <- maps:keys(Attrs)].
