Off-side rule
=============

Indentation instead of braces and semicolons.

Exists, with some differences, in

* Haskell: http://www.haskell.org/onlinereport/lexemes.html#sect2.7
* Python
* CoffeeScript
* Some unimplemented language by Peter J. Landin

Examples of white-space in Haskell:

``` Haskell
if a then do b <- if c then 1
                       else 2
             return b + 3
     else do return 4
```

Maybe useful links:

* http://en.wikipedia.org/wiki/Off-side_rule
* http://stackoverflow.com/questions/356638/how-would-you-parse-indentation-python-style
* http://stackoverflow.com/questions/232682/how-would-you-go-about-implementing-off-side-rule
