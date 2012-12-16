Language design overview
========================

These are some main guidelines for the language.

* Simple
* Explicit
* Intuitive
* Efficient

The language specification is independent from any implementation, but every
choice is done with implementation in mind.  It should be *possible* to
implement efficiently.

The language specification should not be more specific then necessary.
Specifically, it should not imply how things should be implementation.

For instance, the *value recycling* optimization is not required by a compiler,
though the language is designed with this in mind.  This optimization (afai)
implies ref-counted memory management.

Below follows some design choices and why they are chosen.

Immutable values
----------------

* Allows closures to be implemented without a spaghetti stack.  (Erlang style)
* Forbids cyclic data structures, which in turn makes ref-counted memory
  management work.
* Allows values to be passed by reference.  No concept of references or
  pointers is needed in the language.

Built-in data structures
------------------------

Scalars, arrays and dictionaries (aka hash tables) are identical to those in
JSON.

* JSON is simple and well-known.
* Arrays and hash-tables are efficient and intuitive.

Undesired features
------------------

These feature are undesired:

* Finalizers.  We don't want to imply a certain memory management (ref-counted
  vs. tracing gc).  Control flow should not be mixed with memory management.
* Object-orientated inheritance.  This prevents type-inference and encourages
  construction of complex programs.
