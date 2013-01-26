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
* Type assertions for concatenations in expressions [DONE]
* Documentation of the language and the implementation so far [DONE]
* Fix the directory structure and create repository [DONE]

Milestone 2: Logic operations

* Assertion that a value is boolean, that will raise an error otherwise [DONE]
* Conversion to and from masked and C booleans. [DONE]
* And-or-not in lexer and parser
* Type check and-or-not
* Detect last var accesses and discards, and-or-not (short-circuit evaluation)

Milestone 3: String concatenation in patterns

* Add "fixed" annotation in patterns [DONE]
* Check that at least one operand of ~ in a pattern is fixed [DONE]
* Compile patterns containing string concatenation [DONE]

Milestone 4: Arrays

* Array constuctor syntax "[" ... "]" in the grammar [DONE]
* Array value representation in runtime [DONE]
* Compilation of array construction and pattern [DONE]
* Array concatenation: compilation and basic type error detection
* Add "fixed" annotation on arrays (in patterns) [DONE]
* Compile patterns containing array concatenation [DONE]

Dicts:

* Dict literals and basic dict operations: set, lookup, delete
* Dicts in patterns: `dict{key1: value1, key2: value2, ...}`

Unordered:

* Function application (1-arity)
* BIFs for string IO or argv
* Function def (1-arity)
* String-patterns (regex) with variable binding: `/(?<varname>pattern)/`
* Indentation-aware lexer [DONE]
* Guards
* Case [PARTIAL]
* Try [in | of] catch finally

