Large substrings and sub arrays
===============================

Influenced by how large binaries are handled in Erlang.

* Large substrings are implemented as refs to some binary "blob", with length
  and offset.
* Blobs have a refs list and a ref-counter.
* When only a small part is used, the big blob is freed and the small parts are
  transformed to smaller objects.
* Checking the garbage ratio involves some algorithm to work on intervals.

  struct stringblog {
    int ref_count;
    struct large_string refs[];
    int length;
    char data[1]; /* as large as needed */
  };

  struct large_string {
    struct stringblob *data;
    int offset;
    int length;
  };

Garbage ratio algorithm
-----------------------

It's possible to do it in time linear to the number of references.

Implementation plan
-------------------

Not going to happen in the near future.
