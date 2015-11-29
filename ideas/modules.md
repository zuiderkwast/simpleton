Modules
=======

Each module is a translation unit. That means, it can be compiled separately
from other modules.

Each module is compiled to a C file which is a translation unit in C, thus we
too call it translation unit.

Intermodular dependences
------------------------

Our compiler should be able to infer the dependencies between modules and where
there is mutual dependence, do a by-function dependence inference and compile/
typecheck them one by one in dependency-order.

This can be done concurrently in tasks, one per translation unit, as it was
reported that the Elexir compiler does [CodeMesh, London, December 2013].

If there is mutual dependence between functions, inference can assume the arity
of functions from the function application but it cannot assume anything about
freeing or non-freeing.
