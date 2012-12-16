Value recycling
===============

This is an in-place update optimization with pre-requisits:

* The last access of every variable can be detected
* Values are reference-counted

Implementation
--------------

All operators have 1 or more inputs and 1 output value.  Each op has to free
its input afterwards if their ref-counters are zero.  The output value should
be left with its ref-counter potentially zero.  Instead of freeing a value's
memory, it may be modified and reused as the output value.

Variable access "v":

1. Decrement ref-counter iff this is detected to be the last access of the var;
2. The value is the output, possibly with a ref-counter at zero;

Concatenation, "a ++ b":

1. compute result, re-using a or b as the output if their ref-counter is zero;
2. free a and b if their ref-counters are zero and they are not used as the
   return value.

Assignment, "v = a":

1. Assign a to v;
2. Increment ref-counter of a (or of v, equivallently);
3. The output is a (or v equivallently);

Sequencing, "a; b":

1. Free a if its ref-counter is zero;
2. the output is b

