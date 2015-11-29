Type system ideas
=================

A fixed set of types
--------------------

Guideline: Type annotations should never be necessary.  Therefore, they should
not be part of the language, except possibly as documentation.

Idea: A fixed set of types with a defined hierarchy.  The type "any" is valid
for any value, but use a more specific type when it can be infered.

```
  number = integer | float
  scalar = number | string | boolean | null
  data   = scalar | [data] | {data}
  any    = data | [any] | {any} | any -> any | filehandle | (other resources)
                | module | behaviour | ...
```

This implies subtype relations such as number <: scalar <: data <: any, which
should be relatively simple to infer correctly using Damas-Hindly-Milner.
Union types other than the pre-defined ones may not be necessary.

Implementation note: This requires the use of type variables in function types,
to be able to infer things correctly, such as the identity function: a -> a for
any type a.

When the exact type is known at compile time, unboxed values may well be used
by the compiler for some of the scalar types (numbers, booleans, null).

Polymorphism as optimisation
----------------------------

Multiple represenations of values that in the language is the same type.

* Term (unknown type, boxed value);
* fixnum, boolean, double (for unboxed representation).

String types:

* Short strings (up to 6 bytes, encoded in the value without allocation),
* allocated strings,
* string constantes (containing eller consists of string constantes in C).

Array types:

* Array of values, parameterized in the common super-type T of the elements;
* slice of values, parameterized in the element type T;
* array of fixnums (32bit integers)
* array of bytes (for binary data)
* array of fixed length (tuple), parameterized in its length and the type of
  each element. See [tuples](tuples.md) for how this can be used for
  optimizations.

Dict:

* Dict parameterized in the types of the keys and the values;
* Dict with fixed keys and the type of each value...

Type system milestone 1
-----------------------

* use dynamic types
* raise exceptions for type errors in runtime
* use simple monomorphic type inference to detect some obvious type errors at
  compile time.

Type system milestone 2
-----------------------

* Use the Hindley-Milner type system defined above
* Detect more type errors at compile time

Type system milestone 3+
------------------------

```
  tree :: ["node", tree, tree] | "leaf"
```

* Handle fixed sized arrays as tuples, and infer the type of each element
* Add value types as the most specific type for scalars, to be able to use an
  array with a string tag as an algebraic data type.
* Research union and intersection types....
