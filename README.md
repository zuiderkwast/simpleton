Lesser is more
==============

Lesser is a functional programming language project based on the idea of _garbage recycling_, which is an attempt to combine the following ideas:

* Values should be immutable. (This is good for simplicity, correctness, thread-safety etc.)
* Hash tables, arrays and strings are efficient data structures, thus useful language primitives.
* Most values are used only once.

Immutable hash tables and arrays are not the most practical data structurs, since a new copy has to be created for every little change.  However, with code analysis, it is possible to detect the last access of every variable.  This, combined with reference counted objects, makes way for in-place update optimization.  Example:

```
a = [1, 2, 3]
b = append(a, 4)
```

The variable `a` is initialized to an array with 3 elements.  Then `4` is appended to a to produce another array, which is assigned to `b`.  Semantically, this is a new array, but since this is the last use of `a`, the array `a` can be re-used, and instead of copying it to create the new array, `4` can simply be appended the same array, which is then returned as a brand new array.  Semantically, it is not the same array, but since there are no references it, the garbage from `a` can be _recycled_.

The same principle applies to hash tables and strings.

The project
-----------

This is an experimental project, consisting of three parts:

* this documentation,
* a compiler, written in Erlang, which compiles Lesser to C code,
* a runtime, written in C.

The tree are evolving in parallel.

Current status: This README only.  A compiler and runtime with a few features exist, yet to be commited.

Other characteristics
---------------------

This language is in a very early stage and not much has been decided.  The first goal is to create a proof of concept and to evaluate the garbage recycling concept.

A main idea is to keep things simple.  That should be the point of any high-level language.

Aside from the _garbage recycling_ idea, the basic value types and their syntax are borrowed from JSON (http://json.org/) and JavaScript: `"abc"`, `42`, `3.14159`, `true`, `false`, `null`, `[1, 2, 3]`, `{"foo": true, "bar": 42}`.

Other features are borrowed from various languages:

* Dynamic type system (to allow arbitrary data from e.g. parsing JSON, which is usefull in distributed programming, e.g. in communication between a web server and a browser).  Some limited type inference is implemented in the experimental compiler.  Detecting errors at compile time is always desirable, when possible.
* Pattern matching, as used in Haskell and Erlang
* Exceptions (Try-[of|in]-[catch]-[finally])

Syntax
------

An idea is to avoid `[]` and `{}` in the control flow, since these are used in the data.  Overloading these may confuse things.  It is tempting to avoid semicolons and braces using an _off-side rule_, i.e. indentation instead of braces, as in Python and Haskell.

The constructs so far, which may very well change, are

* `expr1 ; expr2` - Evalueare expr1 and discard the result.  Then evaluate expr2.  The result is that of expr2.
* `expr1 ++ expr2` - Array concatenation and string concatenation.
* `if expr1 then expr2 else expr3` - Functional, like the `:?` construct in C, Java etc.
* `pattend = expression` - Match an expression agains a patten an bind the free variables in pattern.  Raises an exception if the matching fails.

A pattern is an expression without any computational operations.  It must be able to do data decomposition.  Decomposition should be similar to data composition.  Example: In `[ "foo", a, b ] = [ "foo", 2, 3 ]`, the array on the right is matched agains the pattern on the left and the variables `a` and `b` are bound to the elements of the array.  Something similar for dicts (hash-tables) needs to be invented...

Ideas for more syntax:

* _Case_, with pattern matching (like in Erlang and Haskell)
* _Try (of/in) catch finally_, with pattern matching in the `of` and in the `catch` clauses. (Erlang style)
* _Function_ definitions and function applications, with parentheses.  Probably something similar to JavaScript, but with pattern matching.

