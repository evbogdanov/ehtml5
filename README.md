## Simple Erlang Template Engine

Example:

```erlang
[html, #{lang => <<"en">>}, [
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
]].
```

becomes

```html
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8">
    <title>EHTML5</title>
    <script type="text/javascript">
      if (foo) bar(1 + 2);
    </script>
  </head>
  <body>
    <h1>EHTML5</h1>
    <div class="col" id="container">
      <p>What is EHTML5?</p>
      <p>EHTML5 is a simple template engine inspired by Yaws's EHTML.</p>
    </div>
  </body>
</html>
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

## Compiling
```
make
```

## Running Tests
```
make tests
```
