Encoding function names in C
============================

*(Actually we don't strictly need this if we don't want to overload multiple
versions of the same function.)*

First the prefix "`_Y`", then for each each parameter *x<sub>i</sub>* the string
`"f"` if the function is freeing *x<sub>i</sub>* or the string `"n"` if it's
non-freeing. Thereafter an underscored followed by the original function name.
Example: A function that takes three arguments och frees the last two should
have the prefix `_Ynff_"`. The original name is the name of the function in the
source language, adjusted so that it doesn't contain any invalid characters and
not longer than the maximum function name length ackording to the C standards.

I chose the prefix `"_Y"` because C++ is using `"_Z"` as the prefix and `Y`
is the letter next to `Z` in the alphabet. Prefixes with underscore and a
capital are reserved for something which seemed appropriate for C++ encoded
function names. In C++ the functions are prefixed with `"_Z"` followed by the
types of the parameters so that the function name can be overloaded for
different sets of parameters.

See also
--------

* [Value recycling](value-recycling.md) - how to compile function application
  and definition
* [Closures](closures.md) - what data we need to store in memory for a closure
