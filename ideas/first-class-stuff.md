First class functions, modules and behaviours
=============================================

First class stuff that might be needed:

* Functions (including closures)
* File handles, sockets and other opaque resources
* Modules (a module is a set of functions)
* Behaviours (a specification for a module, with functions and their types)

Global, built in, stuff that might be needed:

* Modules
* Behaviours
* Functions (perhaps as an implicit core module)

Global behaviours and modules could be accessed using a namespace, s.t. an URI,
and URN with a hash etc.

Functions, file handles etc. could all be encoded as transparent data values,
but they must not be forgable.  It is usefule however, to be able to ensure
certain properties using a type system.  If, for instance, filehandle variables
are distinct from data variables and a function open() returns a filehandle on
success and throws exception on failure, it is impossible to ever reach a point
where we have an invalid filehandle in a variable.

Don't we need to be able to mix functions and resources with scalars in data
structures? Yes we do.  See the type system ideas in type-system.md.

Usefule case: jQuery's ajax function.  It takes a map where some of the values
are scalars and others are callback functions.
