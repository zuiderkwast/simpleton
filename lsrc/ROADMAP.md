Roadmap
=======

Milestone 1: Strings and basic structure
-----------

Strings, concatenaion, booleans, pattern as a single variable, type tagged
values.

* String literals prototype (no escapes) in lexer and parser [DONE]
* String literals representation [DONE]
* Ref-counted strings in runtime, with test [DONE]
* Non-literal strings, compiler [DONE]
* String concatenation: compilation and basic type error detection [DONE]
* Value recycling for string concatenation [DONE]
* Pattern matching an unbound variable as the pattern [DONE]
* Pattern matching a literal or bound varialbe as the pattern, typechecker [DONE]
* Pattern matching with literal or bound variable, compiler [DONE]
* Matching expression ::= pattern match-operator expression (match or fail)
  [DONE]
* Sequencing ";" [DONE]
* Boolean literals [DONE]
* If-then-else [DONE]
* Representation of boolean values (masked in a pointer) [DONE]
* Infer the last access of every variable [DONE]
* Consistent discard of variables in unused branches, if-then-else [DONE]
* Type assertions for concatenations in expressions
* Documentation of the language and the implementation so far
* Fix the directory structure and create repository [DONE]

Milestone 2: Logic operations

* Assertion that a value is boolean, that will raise an error otherwise [DONE]
* Conversion to and from masked and C booleans. [DONE]
* And-or-not in lexer and parser
* Type check and-or-not
* Detect last var accesses and discards, and-or-not (short-circuit evaluation)

Milestone 3: String concatenation in patterns

* Add "size known" annotation on strings (in patterns)
* Check that at least one operand of ++ in a pattern has a fixed or computable
  length
* Compile patterns containing string concatenation

Milestone 4: Arrays

* Array constuctor syntax "[" ... "]" in the grammar
* Array value representation in runtime
* Array concatenation: compilation and basic type error detection
* Add "length known" annotation on arrays (in patterns)
* Compile patterns containing array concatenation (overloaded string concat
  operator)

Dicts:

* Dict literals and basic dict operations: set, lookup, delete
* Dicts in patterns: `dict{key1: value1, key2: value2, ...}`

Unordered:

* Function application (1-arity)
* BIFs for string IO or argv
* Function def (1-arity)
* String-patterns (regex) with variable binding: `/(?<varname>pattern)/`
* Indentation-aware lexer
* Guards
* Case
* Try [in | of] catch finally

