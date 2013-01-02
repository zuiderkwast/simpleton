/* Runtime for lsrc compiled programs */

#include "common.h"
#include "string.h"
#include "array.h"

/*-------------------------------------------------------------------
 * Functions that need to know about all types: strings, arrays etc.
 *-------------------------------------------------------------------*/

/* free if ref-counter == 0 */
static inline void lsr_free_unused(lsr_t *ptr) {
	if (lsr_is_masked(ptr) || !lsr_ptr_is_refcounted(ptr))
		return; /* nothing to free */
	if (ptr->refc != 0) return;
	/* we wouldn't need know the type unless we wanted to track memory usage */
	switch (lsr_type(ptr)) {
		case LSR_STRING:
			lsr_free(ptr, lsr_sizeof_string(((lsr_string_t *)ptr)->len));
			break;
		case LSR_ARRAY:
			lsr_free(ptr, lsr_sizeof_array(((lsr_array_t *)ptr)->cap));
			break;
		default:
			lsr_type_error(ptr, "lsr_free_unused");
	}
}

/* discard a reference to a value. may free the data */
static inline void lsr_discard_ref(lsr_t *ptr) {
	lsr_decref(ptr);
	lsr_free_unused(ptr);
}

/*
 * Compare any two values for equality.
 */
static inline bool lsr_equals(lsr_t *a, lsr_t *b) {
	if (a == b)
		return true;
	/* masked (boolean, int, null) in the pointer */
	if (lsr_is_masked(a) || lsr_is_masked(b))
		return false;
	/* strings (literals and allocated) */
	if (lsr_is_string(a) && lsr_is_string(b))
		return strcmp(lsr_chars(a), lsr_chars(b)) == 0;
	return false;
}
