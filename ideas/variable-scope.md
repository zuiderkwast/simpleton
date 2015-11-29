Variable scope
==============

Desired property: At every point, we know at compile time which variables are
bound and which are free.  (This is not the case in Erlang.)

Which constructs should define a local scope?

Since variables are bound using pattern matching, it is relevant to consider
where patterns can occur:

* using the matching operator `=`
* case clause
* try clause
* catch clause
* function clause

One might want to use the same variable name in different clauses in a case
expression, for different types of data.

```
  case x of
    ["number", n] -> n + 42
    ["name", n]   -> length(n)
```

With this in mind, it makes sense to have clause local scopes.  For the
matching operator, `=`, the scope can be defined to be the rest of that sequence of
expressions.

```
  do x = 42
     y = do z = 9
            x + z
     # Here x = 42, y = 51 and z is free
```

This is similar to a `let` construct in Haskell and ML, but also to the
variable scopes in C.  The scope follows the indentation.

Branching constructs
--------------------

For other branching constructs, such as if-then-else, we have a problem if a
variable is bound in the `then` clause but not in the `else` clause.
Therefore, lets forbid them to bind variables, unless there is a sequence,
which introduces a new local scope.

Example, with `y` local in the `then` clause:

```
  if x > 2 then do { y = x * x; y + 24 } else 0
```

or equivallently, given we have an indentation-aware grammar:

```
  if x > 2 then do y = x * x
  	           y + 24
           else 0
```

What about `and` and `or`, with short-circuit evaluation? The same.
