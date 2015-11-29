Value recycling
===============

This is an in-place update optimization with pre-requisits:

* The last access of every variable can be detected
* Values are reference-counted

Definition of freeing
---------------------

A function is *freeing* means that the function frees the memory owned by its
parameters. Freeing the parameter *y* in *f(y)* means that *f* is responsible
for freeing *y*'s memory before returning if *y*'s reference counter is zero.

How taking responsibility of freeing allows in-place update optimisation
------------------------------------------------------------------------

All operators have zero or more inputs and optionally an output value.  An
operator can have the responisibility of freeing its inputs if their
ref-counters are zero.  The output value should typically be returned with its
ref-counter set to zero.  Instead of freeing an input's memory, it may be
modified and reused as the output value.

How this affects various language features
------------------------------------------

Variable access `v`:

1. Decrement ref-counter iff this is detected to be the last access of the var;
2. The value is the output, possibly with a ref-counter at zero;

Concatenation, string `a ~ b` and array `a @ b`:

1. compute result, re-using a or b as the output if their ref-counter is zero;
2. free a and b if their ref-counters are zero and they are not used as the
   return value.

Assignment, `v = a`:

1. Assign a to v;
2. Increment ref-counter of a (or of v, equivallently);
3. The output is a (or v equivallently);

Sequencing, `a; b`:

1. Free a if its ref-counter is zero;
2. the output is b

Compiling function application
------------------------------

On function application *f(x<sub>1</sub>, ..., x<sub>n</sub>)* do the following:

1. Check that the arity for *f* is *n*;
2. compute or lookup the corresponding function name in C, possibly with
   the types and whether it is freeing or not encoded in the name;
3. call the function and assign the return value to a new variable;
4. for each argument passed to *f* that *f* is not freeing, free the value if
   the ref-counter is zero.

Compiling closure application
-----------------------------

On function applikation *g(x<sub>1</sub>, ..., x<sub>n</sub>)* where *g* is a
variabel we have to check in runtime...

1. that *g* is a closure or function reference and
2. that the arity is *n*.

Whether *x<sub>i</sub>* is freeing is not something we can possibly know in
compile time. Whether we assume that it is freeing or not, we have to do the
memory book keeping in runtime.

Compliling a function definition
--------------------------------

Compile a function definition *f(x<sub>1</sub>, ..., x<sub>n</sub>)* this way:

1. Check what *n* is and for each *i* (1 ≤ *i* ≤ *n*) whether *f* is freeing
   *x<sub>i</sub>*;
2. generate a name containing the original function name and encoded information
   for each parameter whether it is freeing or not.

How to tell whether *f* is freeing *x<sub>i</sub>*
--------------------------------------------------

If a function *f* has a parameter *x* and in its implementation is using *x* in
a call to another function that is freeing *x*, without *f* first incrementing
the reference counter of *x*, then *f* is to be freeing *x*.

