Simplicity
==========

* A language should not be smart. Especially, a language should not try to be smarter than you.

Pros of not having mutable values and variables
-----------------------------------------------

* The uncertainty of whether a function mutates a value or returns another value is a non-issue.
* References and pointers lose their point, which is the ability to mutate the referenced variable, and are therefore useless.
* Call-by-value is the same as call-by-reference, so the pros of both apply.
* Clojures can be easily and safely implemented simply by copying the local variables.


Pros of excluding object orientation
------------------------------------

Object orientation in short:

* Class, a datastructure with a set of functions that operate on the data.
* Method, a function with a dynamic dispatch that depends of its first parameter.
* Subtyping, different implementations of a type and a subtype that easily may break the Liskov substitution principle.
* Inheritence, behaviour implicitly imported from one or more superclasses.

Pros of excluding OO:

* Function dispatch is independent from the parameter values (but not necessarily of their types).
* Much fewer concepts to think about.
