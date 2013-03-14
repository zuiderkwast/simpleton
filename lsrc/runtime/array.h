/*
 * array.h: Array implemented as an aadeque.
 *
 * All functions except lsr_is_array() and lsr_assert_array() assume that their
 * argument of type (lsr_t *) can be safely cast to (aadeque_t *). The value of
 * type (lsr_t *) returned by any of these functions can be safely cast to
 * (aadeque_t *).
 */

#define AADEQUE_HEADER LSR_HEADER;
#define AADEQUE_ALLOC(size) lsr_alloc(size)
#define AADEQUE_REALLOC(ptr, size, oldsize) lsr_realloc(ptr, size, oldsize)
#define AADEQUE_FREE(ptr, size) lsr_free(ptr, size)
#define AADEQUE_MIN_CAPACITY 4
#include "aadeque.h"

static inline bool lsr_is_array(lsr_t *ptr) {
	return !lsr_is_masked(ptr) && ptr->type == LSR_ARRAY;
}

static inline void lsr_assert_array(lsr_t *ptr) {
	if (!lsr_is_array(ptr))
		lsr_error("Array expected.");
}

/*
 * Creates an array with n undefined values
 */
static inline lsr_t *lsr_array_create(unsigned int n) {
	lsr_t *a = (lsr_t *)aadeque_create(n);
	lsr_init_ptr((lsr_t *)a, LSR_ARRAY);
	return a;
}

static inline void lsr_array_free(lsr_t *array) {
	aadeque_destroy((aadeque_t *)array);
}

/*
 * Returns the number of elements in the array.
 */
static inline unsigned int lsr_array_len(lsr_t *array) {
	return aadeque_len((aadeque_t *)array);
}

/*
 * Access an elements in the array.
 */
static inline lsr_t *lsr_array_get(lsr_t *array, unsigned int i) {
	return aadeque_get((aadeque_t *)array, i);
}

/*
 * Insert an elements in the array. This DOES NOT increment the value's
 * ref-counter. That must be done separately.
 */
static inline void lsr_array_set(lsr_t *array, unsigned int i, lsr_t *value) {
	aadeque_set((aadeque_t *)array, i, value);
}

/* Returns a slice of an array. Reuses the array if possible. */
static inline lsr_t *lsr_array_slice(lsr_t *array,
                                     size_t offset,
                                     size_t length) {
	aadeque_t *a = (aadeque_t *)array;
	unsigned int off = (unsigned int)offset,
	             len = (unsigned int)length;
	if (array->refc == 0) {
		/* reuse */
		unsigned int last_n = aadeque_len(a) - off - len;
		a = aadeque_delete_last_n(a, last_n);
		a = aadeque_delete_first_n(a, offset);
		return (lsr_t *)a;
	}
	return (lsr_t *)aadeque_slice(a, off, len);
}

/*
 * Concatenares two arrays. Reuses one of them if possible, prefering the longest.
 */
static inline lsr_t *lsr_array_concat(lsr_t *a, lsr_t *b) {
	if (a->refc == 0 && (b->refc != 0 || lsr_array_len(a) >= lsr_array_len(b))) {
		/* reuse a, append b. (b is not reusable or b is shorter than a.) */
		a = (lsr_t *)aadeque_append((aadeque_t *)a, (aadeque_t *)b);
		if (b->refc == 0)
			lsr_array_free(b);
		return a;
	} else if (b->refc == 0) {
		/* reuse b. prepend a to b. */
		b = (lsr_t *)aadeque_prepend((aadeque_t *)b, (aadeque_t *)a);
		if (a->refc == 0)
			lsr_array_free(a);
		return b;
	} else {
		/* must copy */
		unsigned int i,
		             len_a = lsr_array_len(a),
		             len_b = lsr_array_len(b);
		aadeque_t *c = aadeque_create(len_a + len_b);
		for (i = 0; i < len_a; i++)
			aadeque_set(c, i, aadeque_get((aadeque_t *)a, i));
		for (i = 0; i < len_b; i++)
			aadeque_set(c, i + len_a, aadeque_get((aadeque_t *)b, i));
		return (lsr_t *)c;
	}
}

/*
 * Compare an array agains a slice of another array
 */
static inline bool lsr_equals_slice(lsr_t *array, lsr_t *subject,
                                    size_t offset, size_t length) {
	aadeque_t *a = (aadeque_t *)array,
	          *b = (aadeque_t *)subject;
	unsigned int off = (unsigned int)offset,
	             len = (unsigned int)length;
	unsigned int i;
	if (aadeque_len(a) != len)
		return false;
	for (i = 0; i < len; i++)
		if (!lsr_equals(aadeque_get(a, i), aadeque_get(b, i + off)))
			return false;
	return true;
}
