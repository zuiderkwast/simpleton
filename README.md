Simpleton: A simple programming language
========================================

Simpleton is a functional programming language that relies heavily on in-place
update optimizations in an attempt to combine the following:

* Immutable values, single assignment variables.
* Hash tables, arrays and strings as primitive data structures.  Forget about
  linked lists.

Immutable hash tables and arrays are not the most practical data structurs,
since a new copy has to be created for every little change.  However, with code
analysis, it is possible to detect the last access of every variable.  This,
combined with reference counted objects, makes way for massive in-place update
optimizations.

The project
-----------

This is an experimental project, consisting of three parts:

* language documentation,
* a compiler `lsrc`, written in Erlang, which compiles Simpleton to C code,
* a runtime, written in C.

This language is in a very early stage and not much has been decided.  The first
goal is to create a proof of concept and to evaluate the potential of the
concept, i.e. relying on in-place update optimizations.

A main idea is to keep things simple.  That should be the point of any
high-level language.

Current status: See ROADMAP.md in `lsrc`.

Language overview
-----------------

The basic value types and their syntax are borrowed from JSON and JavaScript:
`"abc"`, `42`, `3.14159`, `true`, `false`, `null`, `[1, 2, 3]`, `{"foo": true,
"bar": 42}`.

Other (planned) features are borrowed from various languages:

* Dynamic type system (to allow arbitrary data from e.g. parsing JSON, which is
  usefull in distributed programming, e.g. in communication between a web server
  and a browser).  Some limited type inference is implemented in the
  experimental compiler.  Detecting errors at compile time is always desirable,
  when possible.
* Pattern matching, as used in Haskell and Erlang, in function clauses and case
  constructs
* String concatenation and array concatenation as decomposition in patterns
* Exceptions (`Try`-[`of`|`in`]-`catch`-`finally`), with pattern matching in the
 `of` and `catch` clauses (Erlang style).
* Lexical scope

A _pattern_ is an expression without computational operations.  It must be able
to do data decomposition, though.  Decomposition should be similar to data
composition.  For example: In `[ "foo", a, b ] = [ "foo", 2, 3 ]`, the array on
the right is matched agains the pattern on the left and the variables `a` and
`b` are bound to the corresponding elements of the array.  Something similar for
dicts (hash-tables) needs to be invented...

See the `ideas` directory for some more ideas.


Syntax
------

The constructs so far:

* `expr1 ; expr2` - Evalueate `expr1` and discard the result.  Then evaluate
  `expr2`.  The result is that of `expr2`.
* `expr1 ~ expr2` - String concatenation.
* `expr1 @ expr2` - Array concatenation.
* `if expr1 then expr2 else expr3` - Functional, like the `? :` construct in C.
* `pattend = expression` - Match an expression agains a patten an bind the free
  variables in pattern.  Raises an exception if the matching fails.

An idea is to avoid `[]` and `{}` in the control flow, since these are used in
the data.  Overloading these may confuse things.  It is tempting to avoid
semicolons and braces using an _off-side rule_, i.e. indentation instead of
braces, as in Python and Haskell.

Ideas for more syntax:

* _Function_ definitions and function applications, with parentheses.  Probably
  something similar to JavaScript, but with pattern matching in the definition.
* _Dict cons_: `mydict{"foo": "bar"}` means "`mydict`, with the addition that
  `"foo"` is mapped to `"bar"`. The result is the new dict.  Note that `"foo"`
  is replaced if it already is in `mydict` and added otherwise.  When the same
  syntax is used for deconstruction in a pattern, `"foo"` is always excluded
  from `mydict`.
