## Simple Erlang Template Engine

Example:

```erlang
[html, #{lang => "en"}, [
  [head, [
    [meta, #{charset => "UTF-8"}],
    [title, Title],
    [script, #{type => "text/javascript"}, "if (foo) bar(1 + 2);"]
  ]],
  [body, [
    [h1, "EHTML5"],
    ['div', #{id => "container", class => "col"}, [
      if YouLikeErlang -> [p, "Write HTML in Erlang!"];
         true          -> [p, "Learn you some Erlang for great good!"]
      end,
      [p, "No, really, writing HTML in Erlang is so much fun" ++ " :-)"]
    ]]
  ]]
]].
```

becomes

```html
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8">
    <title>Hello, EHTML5</title>
    <script type="text/javascript">
      if (foo) bar(1 + 2);
    </script>
  </head>
  <body>
    <h1>EHTML5</h1>
    <div class="col" id="container">
      <p>Write HTML in Erlang!</p>
      <p>No, really, writing HTML in Erlang is so much fun :-)</p>
    </div>
  </body>
</html>
```

## Escape
To escape `&`, `<`, `>`, `"`, `'`, and `` ` `` characters use `{tag, Content}` instead of `[tag, Content]`
```erlang
[p, "<script>alert()</script>"] % dangerous
{p, "<script>alert()</script>"} % safe
```

## Usage With Yaws
```erlang
<erl>
out(_Arg) ->
  EHTML5 = [html, [
    [head,
      [title, <<"EHTML5">>]
    ],
    [body,
      [h1, <<"Hello, Yaws">>]
    ]
  ]],
  {html, ehtml5:render(EHTML5)}.
</erl>
```

## Usage With Cowboy
```erlang
handle(Req, State) ->
  EHTML5 = [html, [
    [head,
      [title, <<"EHTML5">>]
    ],
    [body,
      [h1, <<"Hello, Cowboy">>]
    ]
  ]],
  {ok, Req2} = cowboy_req:reply(
    200,
    [{<<"content-type">>, <<"text/html; charset=utf-8">>}],
    ehtml5:render(EHTML5),
    Req
  ),
  {ok, Req2, State}.
```

## Compiling
```
make
```

## Running Tests
```
make tests
```
