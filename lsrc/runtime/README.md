Runtime notes
=============

Representation of string literals
---------------------------------

Since string literals are not aligned, we pad them with 4 bytes
`"\04\03\02\01"` and round the address up.  The resulting `char *` is aligned
so that the last two bits are `00`.

When the value is accessed, the first bytes tells us how many bytes to skip to
get the real string contents.

The trick is illustrated by the following C program:

``` C
#include <stdio.h>

#define lit(str) ((char *)(((size_t)("\04\03\02\01" str) + 3) & ~3))

static inline char * lit_chars(char * s) {
  return s + s[0];
}

int main() {
  char * obj1 = lit("foo");
  char * obj2 = lit("j");
  char * obj3 = lit("junk");
  char * obj4 = lit("banana");
  printf("%s\n", lit_chars(obj1));
  printf("%s\n", lit_chars(obj2));
  printf("%s\n", lit_chars(obj3));
  printf("%s\n", lit_chars(obj4));
}
```
