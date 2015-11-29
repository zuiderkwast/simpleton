Function calls: normal calls and tail calls
===========================================

Normal call
-----------

```
function foo(x) ->
	y = "baz"
	r = case x of
		[y, z, "foo"] -> 9
		[1, 2, w] -> bar(w)
	r + 2
```

Nothing special; just make the call where it stands.

Tail call
---------

```
function foo(x) ->
	y = "baz"
	case x of
		[y, z, "foo"] -> bar(9)
		[1, 2, u, v]  -> baz(u, v)
		_             -> "done"
```

Here we need to free y, u and v before we bar or bas is called. Instead of a
direct return, we need to create a continuation that is called in the end of the
function, after the cleanup after the case construct.

If all functions are 1-ary, we only need to set a function pointer *f* and a
parameter *p* and in the end `return f(p)`.

To be able to handle N-ary function calls, let N be the largest arity of the
possible tail calls, in this case 2. Create three variables: tailcall, p1 and
p2. To schedule a call to bar, set tailcall = 1 and p1 = 9. For baz, set
tailcall = 2, p1 = u and p2 = v. Otherwise, let tail be 0. Then exit the case
normally. After the case cleanup, gerenrate a switch with the appropriate calls
to bar and baz, which may look like this in C:

``` C
	switch (tailcall) {
		case 1: return bar(p1);
		case 2: return baz(p1, p2);
		default: return tmp; /* here tmp is "done" from the 3rd case */
	}
```
