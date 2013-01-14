/* Runtime for lsrc compiled programs */

#include "common.h"

/* functions needed by string.h, array.h etc */
static inline bool lsr_equals(lsr_t *a, lsr_t *b);

#include "string.h"
#include "array.h"

/*-------------------------------------------------------------------
 * Functions that need to know about all types: strings, arrays etc.
 *-------------------------------------------------------------------*/

static inline void lsr_free_unused(lsr_t *ptr);
static inline void lsr_discard_ref(lsr_t *ptr);
static inline char lsr_type(lsr_t *ptr);
static inline lsr_t *lsr_to_json(lsr_t *ptr);

/* free if ref-counter == 0 */
static inline void lsr_free_unused(lsr_t *ptr) {
	if (lsr_is_masked(ptr) || !lsr_ptr_is_refcounted(ptr))
		return; /* nothing to free */
	if (ptr->refc != 0) return;
	/* we wouldn't need know the type unless we wanted to track memory usage */
	switch (ptr->type) {
		case LSR_STRING:
			lsr_free(ptr, lsr_sizeof_string(((lsr_string_t *)ptr)->len));
			break;
		case LSR_ARRAY:
			{
				unsigned i;
				for (i = 0; i < lsr_array_len(ptr); i++) {
					lsr_t *x = lsr_array_get(ptr, i);
					lsr_discard_ref(x);
				}
				lsr_array_free(ptr);
			}
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

/*
 * Returns the type of any value.
 * For string literals, LSR_STRING is returned.
 */
static inline char lsr_type(lsr_t *ptr) {
	if (!lsr_is_masked(ptr)) {
		if (lsr_ptr_is_string_const(ptr)) return LSR_STRING;
		return ptr->type;
	}
	if (lsr_is_bool(ptr)) return LSR_BOOL;
	lsr_error("Unimplemented type in lsr_type");
}


/* Recursive helper for to_json(). Destructively appends to *str. */
static void lsr_append_json(lsr_t **str, lsr_t *ptr) {
	size_t len = lsr_strlen(*str);
	char *chars;
	switch (lsr_type(ptr)) {
	case LSR_STRING:
		{
			size_t n = lsr_strlen(ptr);
			/* No escaping yet. Just wrap "" around the contents (FIXME) */
			/* Make space for the contents and "" */
			*str = lsr_reuse_string(*str, len + n + 2);
			chars = lsr_chars(*str);
			lsr_chars(*str)[len++] = '"';
			strcpy(chars + len, lsr_chars(ptr));
			len += n;
			lsr_chars(*str)[len] = '"';
		}
		break;
	case LSR_ARRAY:
		{
			unsigned int n = lsr_array_len(ptr);
			unsigned int i;
			*str = lsr_reuse_string(*str, ++len);
			lsr_chars(*str)[len - 1] = '[';
			for (i = 0; i < n; i++) {
				if (i) {
					*str = lsr_reuse_string(*str, ++len);
					lsr_chars(*str)[len - 1] = ',';
				}
				lsr_append_json(str, lsr_array_get(ptr, i));
				len = lsr_strlen(*str);
			}
			*str = lsr_reuse_string(*str, ++len);
			lsr_chars(*str)[len - 1] = ']';
		}
		break;
	case LSR_BOOL:
		if (lsr_bool_value(ptr)) {
			*str = lsr_reuse_string(*str, len + 4);
			strcpy(lsr_chars(*str) + len, "true");
		}
		else {
			*str = lsr_reuse_string(*str, len + 5);
			strcpy(lsr_chars(*str) + len, "false");
		}
		break;
	default:
		*str = lsr_reuse_string(*str, len + 13);
		strcpy(lsr_chars(*str) + len, "unimplemented");
	}
}

static inline lsr_t *lsr_to_json(lsr_t *ptr) {
	lsr_t *dst = lsr_create_string(0, "");
	lsr_append_json(&dst, ptr);
	return dst;
}
