/*
 * Strings.
 *
 * This file is included in runtime.h, where common.h is available.
 *
 * The author disclaims copyright to this source code.
 */

/*
 * String literal, aligned using padding chars and rounding the pointer up
 * to a 4 bytes boundary.
 */
#define lsr_string_literal(str) \
	((lsr_t *)(((size_t)("\04\03\02\01" str) + 3) & ~3))

/*
 * Ref-counted, allocated string.
 */
typedef struct {
	LSR_HEADER;    /* LSR_STRING */
	size_t len;    /* Num bytes excl nul */
	char chars[1]; /* Length = len + 1 */
} lsr_string_t;

static inline bool lsr_ptr_is_string_const(lsr_t *ptr) {
	return ptr->type <= '\04';
}

static inline bool lsr_ptr_is_string(lsr_t *ptr) {
	return ptr->type <= '\05';
}

static inline bool lsr_is_string(lsr_t *ptr) {
	return !lsr_is_masked(ptr) &&
	       (lsr_ptr_is_string_const(ptr) || lsr_type(ptr) == LSR_STRING);
}

static inline void lsr_assert_string(lsr_t *ptr) {
	if (lsr_is_masked(ptr) || !lsr_ptr_is_string(ptr))
		lsr_error("String expected.");
}

/*
 * Size of allocated string object (including header) by num chars
 */
static inline size_t lsr_sizeof_string(size_t len) {
	return sizeof(lsr_string_t) + len;
}

/* the pointer must be a string const */
static inline char * lsr_string_const_chars(lsr_t *ptr) {
	char *s = (char *)ptr;
	return s + s[0];
}

/* returns a pointer to the char array in a tagged string */
static inline char * lsr_chars(lsr_t *ptr) {
	if (lsr_ptr_is_string_const(ptr))
		return lsr_string_const_chars(ptr);
	if (lsr_type(ptr) == LSR_STRING)
		return ((lsr_string_t *)ptr)->chars;
	lsr_type_error(ptr, "lsr_chars");
}

static inline size_t lsr_strlen(lsr_t *ptr) {
	if (lsr_ptr_is_string_const(ptr))
		return strlen(lsr_string_const_chars(ptr));
	if (lsr_type(ptr) == LSR_STRING)
		return ((lsr_string_t *)ptr)->len;
	lsr_type_error(ptr, "lsr_strlen");
}

/*
 * Allocates a string of length (capacity) len, refc 0 and
 * copies (nul-terminated) chars.
 */
static inline lsr_t *lsr_create_string(size_t len, const char *chars) {
	lsr_string_t *s = (lsr_string_t *)lsr_alloc(lsr_sizeof_string(len));
	lsr_init_ptr((lsr_t *)s, LSR_STRING);
	s->len  = len;
	strncpy(s->chars, chars, len);
	s->chars[len] = '\0';
	return (lsr_t *)s;
}

/*
 * Change the length (capacity) of an unused string (refc == 0) using realloc.
 */
static inline lsr_t *lsr_reuse_string(lsr_t *ptr, size_t len) {
	lsr_string_t *s = (lsr_string_t *)ptr;
#ifdef LSR_DEBUG
	if (lsr_is_masked(ptr) || ptr->type != LSR_STRING)
		lsr_type_error(ptr, "lsr_reuse_string");
	if (ptr->refc != 0)
		lsr_error("Non-zero refc in lsr_reuse_string");
#endif
	s = (lsr_string_t *)lsr_realloc(s, lsr_sizeof_string(len),
	                                lsr_sizeof_string(s->len));
	s->len = len;
	return (lsr_t *)s;
}

/*
 * Returns a pointer to a new string, with refc == 0.
 *
 * Frees a and b if they are unused (i.e. if their refc == 0).
 */
static inline lsr_t *lsr_string_concat(lsr_t *a, lsr_t *b) {
	size_t len = lsr_strlen(a) + lsr_strlen(b);
	lsr_t *c;
	if (a->type == LSR_STRING && a->refc == 0) {
		/* use a and reallocate to make space for b */
		c = lsr_reuse_string(a, len);
	}
	else {
		/* a is not unused */
		/* TODO: If b is unused, reuse that and prepend a */
		/* created a new string with the capacity and copy the data of a */
		c = lsr_create_string(len, lsr_chars(a));
	}
	/* Append b to c. Todo: memcpy, since we know the lengths. */
	strcat(lsr_chars(c), lsr_chars(b));
	/* Free b if unused */
	if (b->type == LSR_STRING && b->refc == 0) {
		lsr_free(b, lsr_sizeof_string(((lsr_string_t *)b)->len));
	}
	return (lsr_t *)c;
}

static inline lsr_t *lsr_concat(lsr_t *a, lsr_t *b) {
	/* strings only, so far. arrays to come. */
	lsr_assert_string(a);
	lsr_assert_string(b);
	return lsr_string_concat(a, b);
}

/* a and b must be strings. no type check. */
static inline bool lsr_equals_substr(lsr_t *a,
                                     lsr_t *b, size_t b_offset, size_t len) {
	return lsr_strlen(a) == len &&
		strncmp(lsr_chars(a), lsr_chars(b) + b_offset, len) == 0;
}

/* returns a substring. no type check. */
static inline lsr_t * lsr_substr(lsr_t *ptr, size_t offset, size_t len) {
	char * chars = lsr_chars(ptr);
	if (ptr->type == LSR_STRING && ptr->refc == 0) {
		/* Move the contents to the beginning of the string and shrink it */
		memmove(chars, chars + offset, len);
		chars[len] = '\0';
		return lsr_reuse_string(ptr, len);
	}
	else {
		lsr_decref(ptr);
		return lsr_create_string(len, chars + offset);
	}
}
