/* runtime test */

#define LSR_MONITOR_ALLOC
#include "runtime.h"

int verbose = 1;

void test(int cond, const char * msg);
void testLast();
void test1(void);
void test2(void);
void test3(void);
void test4(void);

int main() {
	test1();
	test2();
	test3();
	test4();
	testLast();
	return 0;
}


/*
 * Arrays
 */
void test4(void) {
	int i, ok;
	lsr_t *arr;
	lsr_t *vals[5];

	printf("=== Test 4: Arrays ===\n");

	/* create values to insert */
	vals[0] = lsr_string_literal("a");
	vals[1] = lsr_create_string(1, "b");
	vals[2] = lsr_string_literal("c");
	vals[3] = lsr_string_literal("d");
	vals[4] = lsr_create_bool(true);

	/* create array in the way it would be done in an array expression */
	arr = lsr_array_create(5);
	for (i = 0; i < 5; i++) {
		lsr_array_set(arr, i, vals[i]);
		lsr_incref(vals[i]);
	}

	test(arr->type == LSR_ARRAY, "Type is array");

	/* check contents against expected values */
	for (i = 0, ok = true; i < 5; i++)
		ok = ok && lsr_equals(lsr_array_get(arr, i), vals[i]);
	test(ok, "Array contents as expected");

	/* free array (including contents) */
	lsr_free_unused(arr);
}

/*
 * Equality checks
 */
void test3(void) {
	lsr_t *s1 = lsr_create_string(3, "foo"),
          *s2 = lsr_string_literal("foo"),
          *b  = lsr_create_bool(true);
	printf("=== Test 3: equality checks ===\n");
	test(!lsr_equals(b, s1), "Boolean != string");
	test(lsr_equals(s1, s2), "Allocated string 'foo' == Literal 'foo'");
	lsr_free_unused(s1);
}

/*
 * Booleans, masked in a pointer
 */
void test2(void) {
	lsr_t *t = lsr_create_bool(true),
	      *f = lsr_create_bool(false);
	printf("=== Test 2: true and false ===\n");
	lsr_ensure_boolean(t);
	lsr_ensure_boolean(f);
	test(lsr_bool_value(t), "Boolean true");
	test(!lsr_bool_value(f), "Boolean false");
}

/*
 * Concatenations, with all ref-counters left == 0.
 *
 * Pseudo source code: "hej" + "du" + "du" + "du".
 */
void test1(void) {
	lsr_t *a, *b, *c, *d, *e;
	size_t mem;
	printf("=== Test 1: ref-counted strings and string literals ===\n");

	if (verbose)
		printf("Size of lsr_string_t: %lu\n",
		       sizeof(lsr_string_t));

	test(lsr_alloc_bytes_cnt == 0, "Zero allocation at start");

	a = lsr_string_literal("hej");
	b = lsr_string_literal("du");

	test(lsr_strlen(a) == 3,
	     "Length of 'hej' == 3");
	test(lsr_ptr_is_string_const(a),
	     "The string a is a literal");

	/* c = a + b */
	c = lsr_string_concat(a, b);

	test(lsr_strlen(c) == 5, "Length of 'hejdu' == 5");
	test(c->type == LSR_STRING, "Type of c is non-literal string");
	test(lsr_get_refc(c) == 0, "Refc of c == 0");

	/* Mem before d = c + b */
	mem = lsr_alloc_bytes_cnt;

	/* d = c + b, may result i c reused as d */
	d = lsr_string_concat(c, b);

	mem = lsr_alloc_bytes_cnt - mem;
	test(mem == 2, "Append 2 bytes to existing string allocates 2 bytes");
	test(d == c, "Append 'du' to unused string 'hejdu' re-uses the string");
	test(lsr_strlen(d) == 7, "Length of 'hejdudu' == 7");

	/* last use of d: e = d + b, may result in d being reused as e */
	e = lsr_string_concat(d, b);
	test(e != d, "Append 'du' to unused string 'hejdudu' causes allocation");

	if (verbose)
		printf("Allocation %d times so far, totally %d bytes\n",
		       lsr_num_allocs, lsr_alloc_bytes_cnt);

	/* discard_ref is noop on literals */
	lsr_free_unused(a);
	lsr_free_unused(b);

	/* discard_ref on allocated string */
	lsr_free_unused(e);
}

void testLast(void) {
	printf("=== Final checks ===\n");
	test(lsr_alloc_bytes_cnt == 0, "Zero bytes allocated after cleanup");
	if (verbose)
		printf("Total num allocations: %d\n", lsr_num_allocs);
}

/*
 * Test helper, OK or FAIL depending on the condition to test.  Also prints a
 * message describing what was tested.
 */
void test(int cond, const char * msg) {
	if (!cond) {
		printf("%-70s [FAIL]\n", msg);
		/* exit(2); */
	}
	else if (verbose) {
		printf("%-70s [ OK ]\n", msg);
	}
}

