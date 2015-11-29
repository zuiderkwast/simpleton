Implementation notes
====================

Case
----

The eager reference-counting rule: Delete references to values as early as
possible.

A consequence of this rule is that code that frees a variable has to be repeated
in every branch of the program where the variable is not used.

```
case TEST of
  PATTERN_1 -> RESULT_1
  PATTERN_2 -> RESULT_2
  ...
```

When evaluating a case construct as the one above, the TEST expression is
evaluated and stored in a variable TESTVAR. This has to be deleted as soon as
any pattern matches as well as before raising the error when no pattern matches.

For variables bound in a pattern PATTERN_N, they are bound only if the value
TESTVAR matches the pattern and thus have to be free'd only in the end of the
corresponding RESULT_N.

Variables bound earlier that have their last access in any of the PATTERN_N or
RESULT_N expressions have to be free'd in every branch where it's not used,
inclufing the "no match" error raising case.

Optimizations
-------------

Re-order expressions when possible to find these pairs:

* Remove adjacent incref and decref.
* Adjacend pop/push and unshift/shift are rewritten to a replace operation.
* Adjacent push/pop and shift/unshift are ommitted. The value is stored in a
  variable instead.

Exceptions
----------

Using Setjmp/longjmp.

- Need to store the jmp_buf somewhere. Global variable? Works as long as there
is only cooperative threading, but, naaa. Pass the last jmp_buf to all function
calls?
+ A single jump out to the next try-catch.
- Hard to combine with reference counted variables when jumping out of multiple
  variable scopes.

Using goto within functions and special return value from functions. It could be
a pointer masked with a 1 on one of the least significant bits which are 0 for
every piece of allocated memory.

- Requires a check at every function call.
- Possibly multiple jumps between to the next try-catch.
+ Easy to handle reference counters on local variables nicely.

Using tasks that die on error and task local allocation. This is not a normal
try-catch but more like how programs are handled in an OS using exit codes.

- Some book-keeping necessary.
+ Single jump on error.
± Nice property for those who don't like try-catch.
± Forces routines that may fail to run as separate tasks.
± Fewer language constructs when tasks and error handling are coupled.
