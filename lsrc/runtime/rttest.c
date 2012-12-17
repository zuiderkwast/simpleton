/* runtime test */

#define LSR_MONITOR_ALLOC
#include "runtime.h"

int verbose = 1;

void test(int cond, const char * msg);
void test1(void);
void test2(void);
void test3(void);

int main() {
	test1();
	test2();
	test3();
	return 0;
}

/*
 * Equality checks
 */
void test3(void) {
	lsr_tagged_t *s1 = lsr_create_string(3, "foo"),
	             *s2 = lsr_string_literal("foo"),
	             *b  = lsr_bool_to_ptr(true);
	printf("=== Test 3: equality checks ===\n");
	test(!lsr_equals(b, s1), "Boolean != string");
	test(lsr_equals(s1, s2), "Allocated string 'foo' == Literal 'foo'");
}

/*
 * Booleans, masked in a pointer
 */
void test2(void) {
	lsr_tagged_t *t = lsr_bool_to_ptr(true),
	             *f = lsr_bool_to_ptr(false);
	printf("=== Test 2: true and false ===\n");
	lsr_ensure_boolean(t);
	lsr_ensure_boolean(f);
	test(lsr_ptr_to_bool(t), "Boolean true");
	test(!lsr_ptr_to_bool(f), "Boolean false");
}

/*
 * Concatenations, with all ref-counters left == 0.
 *
 * Pseudo source code: "hej" + "du" + "du" + "du".
 */
void test1(void) {
	lsr_tagged_t *a, *b, *c, *d, *e;
	size_t mem;
	printf("=== Test 1: ref-counted strings and string literals ===\n");

	if (verbose)
		printf("Size of struct lsr_string_nodata: %lu\n",
		       sizeof(struct lsr_string_nodata));

	test(lsr_alloc_bytes_cnt == 0, "Zero allocation at start");

	a = (lsr_tagged_t *) LSR_STRING_CONST_PREFIX "hej";
	b = (lsr_tagged_t *) LSR_STRING_CONST_PREFIX "du";

	test(lsr_strlen(a) == 3,
	      "Length of 'hej' == 3");
	test(a->type == LSR_STRING_CONST,
	      "The string a is a literal");

	/* c = a + b */
	c = lsr_string_concat(a, b);

	test(lsr_strlen(c) == 5, "Length of 'hejdu' == 5");
	test(lsr_type(c) == LSR_STRING, "Type of c is non-literal string");
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
		printf("Total allocation at this point: %d\n", lsr_alloc_bytes_cnt);

	/* discard_ref is noop on literals */
	lsr_free_unused(a);
	lsr_free_unused(b);

	/* discard_ref on allocated string */
	lsr_free_unused(e);

	if (verbose)
		printf("Num allocations: %d\n", lsr_num_allocs);

	test(lsr_alloc_bytes_cnt == 0, "Zero bytes allocated after cleanup");
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

