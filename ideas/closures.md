Function references and closures
================================

Data for a closure
------------------

* Tag (function)
* Refc
* Arity (num args)
* Bitmask or similar encoding which args the function frees/reuses
* Pointer to the function's executable code
* Number of closure variables
* Closure data

See [value recycling](value-recycling.md) for how to implement function
definition and application.
