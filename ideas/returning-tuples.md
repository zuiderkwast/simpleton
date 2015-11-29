Optimizations for fixed length arrays AKA tuples
================================================

Returning fixed length arrays using pointer parameters
------------------------------------------------------

In order to avoid allocating an array for returning short arrays that are used
as tuples, we can return the array elements using pointer parameters.

For instance, we can compile `[min, max] = get_min_and_max(list_of_numbers)` to
the following C code:

```c
di_t min, max;
get_min_and_max(&min, &max, list_of_numbers);
```

In order to know that a function returns a certain number of values using
pointer parameters we need to encode this number in the function name. We can
encode it as a number before the freeing/non-freeing information for the
parameters. If there is a digit in this position, it means that the function
always returns an array of this length *n* with elements pointed to by the first
*n* parameters to the function. It can be 0 for a function returning an empty
array. We can put an underscore in this position if the function does not use
this feature if that makes implementation easier.

The function `get_min_and_max` above could have the encoded name 
`_Y2n_get_min_and_max` in C.

Being able to compile a function in this way assumes that we can infer that the
return type of the function is an array of a fixed length.

We can avoid allocating an array to receive the return value of a function call
if the function application is pattern matched against a array pattern of the
same length. It can be an array literal or another expression that can be
infered to hold an array of length *n*.

Passing fixed length arrays using multiple parameters
-----------------------------------------------------

Do we also want to be able to pass fixed length arrays (tuples) as multiple
parameters to a function when a certain parameter can be infered to be a fixed
length array?

To mark that a parameter is an inline *n*-tuble, we can instead of the character
"f" or "n" put a digit *n* followed by *n* characters ("f", "n" or again a digit
for nested tuples) which would mean that the parameter is an array of length *n*
whose elements are passed on the stack and for each "f" or "n" means that the
function frees it or non-frees it. (A digit *m* here means a nested inline fixed
length array and that another *m* characters are expected.)

This optimization is not as important as the one for returning fixed length
arrays as the function can be written in a way that separate parameters are used
instead. Thus it is possible to send the same data without allocations even
without this optimization.

Type system for compilation
---------------------------

We need a type "fixed length array", i.e. a tuple type. This type is a sub-type
to the type "array of any length".

