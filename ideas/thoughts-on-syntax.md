Haskell-style syntax
--------------------

  case 2 of { 1 -> "A"; 2 -> "B"; 3 -> "C" }

* { ; } for separating cases, definitions etc (let, where, case, do).
* let and where instead of sequencing (pure).
* do for sequencing (monadic)

Desired style
-------------

* if x then y else z (Haskell-style, similar in almost every language except
  Erlang)
* case a of { b -> c; d -> e } (Haskell-style)
* optionally indentation based (Haskell-style)
* do for sequencing (haskell-style), suitable if the language is non-pure

It is easy to add a Haskell style layout rule, except the rule for }-insertion
on syntax error which may not be necessary.

Anonymous functions
-------------------

Multi-clause anonymous function (almost Erlang-style):

  f = function {
    0 -> 1;
    1 -> 1;
    n -> n + 2
  }

With layout rule and usage in e.g. map, there will be a syntax error on the
comma. This could be solved with the Haskell rule that inserts "}" on syntax
error.

  map(function x -> x + 1, [1, 2, 3])    # before layout processing
  map(function {x -> x + 1}, [1, 2, 3])  # after layout processing

Toplevel definitions
--------------------

  export foo, bar
  foo(x,y) = x+y;
  bar(x)   = x+1
