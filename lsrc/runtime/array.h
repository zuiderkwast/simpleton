/*
 * array.h: Array implemented as an aadeque.
 */

#define AADEQUE_HEADER LSR_HEADER;
#define AADEQUE_ALLOC(size) lsr_alloc(size)
#define AADEQUE_REALLOC(ptr, size, oldsize) lsr_realloc(ptr, size, oldsize)
#define AADEQUE_FREE(ptr, size) lsr_free(ptr, size)
#define AADEQUE_MIN_CAPACITY 4
#include "aadeque.h"

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
                                     unsigned int offset,
                                     unsigned int length) {
	aadeque_t *a = (aadeque_t *)array;
	if (array->refc == 0) {
		/* reuse */
		unsigned int last_n = aadeque_len(a) - offset - length;
		a = aadeque_delete_last_n(a, last_n);
		a = aadeque_delete_first_n(a, offset);
		return (lsr_t *)a;
	}
	return (lsr_t *)aadeque_slice(a, offset, length);
}

/*
 * Compare an array agains a slice of another array
 */
static inline bool lsr_equals_slice(lsr_t *array, lsr_t *subject,
                                    unsigned int offset, unsigned int length) {
	aadeque_t *a = (aadeque_t *)array,
	          *b = (aadeque_t *)subject;
	unsigned int i;
	if (aadeque_len(a) != length)
		return false;
	for (i = 0; i < length; i++)
		if (!lsr_equals(aadeque_get(a, i), aadeque_get(b, i + offset)))
			return false;
	return true;
}
