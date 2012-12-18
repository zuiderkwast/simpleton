/* Runtime for lsrc compiled programs */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdbool.h>

/* some error "handling" macros - print message and exit */
#define lsr_error(msg) do{\
		printf("%s\n", msg);\
		exit(-2);\
	}while(0)

#define bad_type_error(ptr, situation) do{\
		printf("Bad type '%c' for %s.\n", (ptr)->type, (situation));\
		exit(-2);\
	}while(0)


/* Define this if you want to monitor lsr_alloc_bytes_cnt */
#ifdef LSR_MONITOR_ALLOC
int lsr_alloc_bytes_cnt = 0;
int lsr_num_allocs = 0;
#endif

static inline void *lsr_alloc(size_t size) {
	void * ptr = malloc(size);
	if (!ptr)
		lsr_error("Out of memory.");
#ifdef LSR_MONITOR_ALLOC
	lsr_alloc_bytes_cnt += size;
	lsr_num_allocs++;
#endif
	return ptr;
}
static inline void *lsr_realloc(void * ptr, size_t size, size_t old_size) {
	void * newptr = realloc(ptr, size);
	if (!newptr) {
		printf("Out of memory.\n");
		exit(-1);
	}
#ifdef LSR_MONITOR_ALLOC
	lsr_alloc_bytes_cnt += size - old_size;
	if (newptr != ptr) lsr_num_allocs++;
#endif
	return newptr;
}

static inline void lsr_free(void * ptr, size_t size) {
	free(ptr);
#ifdef LSR_MONITOR_ALLOC
	lsr_alloc_bytes_cnt -= size;
#endif
}

/*
 * The common head of all boxed types.
 */
typedef struct {
	char type;
} lsr_tagged_t;

/* Type tags for the tagged type (char) */
/* string literals:             '\01' .. '\04' */
#define LSR_STRING              '\05'

/*
 * Pointers have 00, while masked types have a 1 in one of the two least
 * significant bits.
 */
#define lsr_is_masked(ptr) ((size_t)(ptr) & 3)

#define lsr_ptr_is_string_const(ptr) ((ptr)->type <= '\04')

/*
 * Booleans, stored inside pointers as 01111x10, where x is 1 for true,
 * 0 for false.  In ascii, these are '>' (true) and ':' (false).
 */

/*
 * Convert a pointer to boolean.  If it's not a boolean, the result is
 * undefined.  Use lsr_is_boolean if you need to check in advance.
 */
static inline bool lsr_ptr_to_bool(lsr_tagged_t * ptr) {
	size_t n = (size_t)ptr;
	return (bool)(n & ~':');
}

static inline lsr_tagged_t * lsr_bool_to_ptr(bool yes) {
	return (lsr_tagged_t *)(size_t)(yes ? '>' : ':');
}

static inline bool lsr_is_boolean(lsr_tagged_t * ptr) {
	return ((size_t)(ptr) & ':') == ':';
}

/*
 * Check if a pointer contains a masked boolean and raise an error if not.
 */
static inline void lsr_ensure_boolean(lsr_tagged_t * ptr) {
	if (!lsr_is_boolean(ptr))
		lsr_error("Non-boolean value in lsr_ensure_boolean");
}

/*
 * Ref-counted, allocated string.
 */
#define LSR_STRING_HEAD \
	char type;     /* 'b' */\
	size_t refc;\
	size_t len    /* Num bytes excl nul */


typedef struct {
	LSR_STRING_HEAD;
	char chars[1]; /* Length = len + 1 */
} lsr_string_t;

struct lsr_string_nodata {
	LSR_STRING_HEAD;
};

/*
 * Size of allocated string object (including header)
 */
#define SIZE_OF_STRING(len) \
	(sizeof(struct lsr_string_nodata) + (len) + 1)

/*
 * Align string literals to 4 bytes boundary using padding and rounding up
 */
#define lsr_string_literal(str) \
	((lsr_tagged_t *)(((size_t)("\04\03\02\01" str) + 3) & ~3))

/*
 * general stuff for lsr_tagged_t
 */

#define lsr_type(ptr) ((ptr)->type)

/* free if ref-counter == 0 */
static inline void lsr_free_unused(lsr_tagged_t *ptr) {
	if (lsr_is_masked(ptr) || lsr_ptr_is_string_const(ptr))
		return; /* nothing to free */
	switch (lsr_type(ptr)) {
		case LSR_STRING: {
			lsr_string_t *s = (lsr_string_t *)ptr;
			if (s->refc == 0) lsr_free(s, SIZE_OF_STRING(s->len));
			break;
		default:
			bad_type_error(ptr, "lsr_free_unused");
		}
	}
}

/* ref-counter manipulation */
static inline void lsr_refc_add(lsr_tagged_t *ptr, size_t delta) {
	if (lsr_is_masked(ptr) || lsr_ptr_is_string_const(ptr))
		return; /* nothing to free */
	switch (lsr_type(ptr)) {
		case LSR_STRING:
			((lsr_string_t *)ptr)->refc += delta;
			break;
		default:
			bad_type_error(ptr, "lsr_refc_add");
	}
}
static inline void lsr_incref(lsr_tagged_t *ptr) {
	lsr_refc_add(ptr, 1);
}
static inline void lsr_decref(lsr_tagged_t *ptr) {
	lsr_refc_add(ptr, -1);
}
/* discard a reference to a value. may free the data */
static inline void lsr_discard_ref(lsr_tagged_t *ptr) {
	lsr_decref(ptr);
	lsr_free_unused(ptr);
}
/* ref-counter manipulation */
static inline size_t lsr_get_refc(lsr_tagged_t *ptr) {
#ifdef LSR_DEBUG
	if (lsr_type(ptr) != LSR_STRING)
		bad_type_error(ptr, "lsr_get_refc");
#endif
	return ((lsr_string_t *)ptr)->refc;
}


/* string functions */

static inline bool lsr_is_string(lsr_tagged_t *ptr) {
	return !lsr_is_masked(ptr) &&
	       (lsr_ptr_is_string_const(ptr) || lsr_type(ptr) == LSR_STRING);
}

/* the pointer must be a string const */
static inline char * lsr_string_const_chars(lsr_tagged_t *ptr) {
	char *s = (char *)ptr;
	return s + s[0];
}

/* returns a pointer to the char array in a tagged string */
static inline char * lsr_chars(lsr_tagged_t *ptr) {
	if (lsr_ptr_is_string_const(ptr))
		return lsr_string_const_chars(ptr);
	if (lsr_type(ptr) == LSR_STRING)
		return ((lsr_string_t *)ptr)->chars;
	bad_type_error(ptr, "lsr_chars");
}

static inline size_t lsr_strlen(lsr_tagged_t *ptr) {
	if (lsr_ptr_is_string_const(ptr))
		return strlen(lsr_string_const_chars(ptr));
	if (lsr_type(ptr) == LSR_STRING)
		return ((lsr_string_t *)ptr)->len;
	bad_type_error(ptr, "lsr_strlen");
}

/*
 * Allocates a string of length (capacity) len, refc 0 and
 * copies (nul-terminated) chars.
 */
static inline lsr_tagged_t *lsr_create_string(size_t len, const char *chars) {
	lsr_string_t * s = (lsr_string_t *)lsr_alloc(SIZE_OF_STRING(len));
	s->type = LSR_STRING;
	s->refc = 0;
	s->len  = len;
	strcpy(s->chars, chars);
	return (lsr_tagged_t *)s;
}

/*
 * Change the length (capacity) of an unused string (refc == 0) using realloc.
 */
static inline lsr_tagged_t *lsr_reuse_string(lsr_tagged_t *ptr, size_t len) {
	lsr_string_t *s;
#ifdef LSR_DEBUG
	if (lsr_type(ptr) != LSR_STRING)
		bad_type_error(ptr, "lsr_reuse_string");
#endif
	s = (lsr_string_t *)ptr;
#ifdef LSR_DEBUG
	if (s->refc != 0)
		lsr_error("Non-zero refc in lsr_reuse_string");
#endif
	s = (lsr_string_t *)lsr_realloc(s, SIZE_OF_STRING(len),
	                                SIZE_OF_STRING(s->len));
	s->len = len;
	return (lsr_tagged_t *)s;
}

/*
 * Returns a pointer to a new string, with refc == 0.
 *
 * Frees a and b if they are unused (i.e. if their refc == 0).
 */
static inline
lsr_tagged_t *lsr_string_concat(lsr_tagged_t *a,
                                lsr_tagged_t *b) {
	size_t len = lsr_strlen(a) + lsr_strlen(b);
	lsr_tagged_t *c;
	if (a->type == LSR_STRING && ((lsr_string_t *)a)->refc == 0) {
		/* use a and reallocate to make space for b */
		c = lsr_reuse_string(a, len);
	}
	else {
		/* a is not unused */
		/* created a new string with the capacity and copy the data of a */
		c = lsr_create_string(len, lsr_chars(a));
	}
	/* Append b to c. Todo: memcpy, since we know the lengths. */
	strcat(lsr_chars(c), lsr_chars(b));
	lsr_free_unused(b);
	return (lsr_tagged_t *)c;
}

static inline
void lsr_print_string(lsr_tagged_t *s) {
	printf("%s", lsr_chars(s));
}

/*
 * Compare any two values for equality.
 */
static inline bool lsr_equals(lsr_tagged_t *a, lsr_tagged_t *b) {
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
