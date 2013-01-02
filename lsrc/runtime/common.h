#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdbool.h>


/*
 * The common header of all boxed types.
 */

#define LSR_HEADER \
	char type;\
	unsigned int refc	

typedef struct {
	LSR_HEADER;
} lsr_t;


/* Type tags for the tagged type (char) */
/* string literals:             '\01' .. '\04' */
#define LSR_STRING              '\05'
#define LSR_ARRAY               '\06'


/* error handling: print message and exit */
#define lsr_error(msg) do {\
		printf("%s\n", msg);\
		exit(-2);\
	} while(0)

#define lsr_type_error(ptr, situation) do {\
		printf("Bad type '%c' for %s.\n", ptr->type, situation);\
		exit(-2);\
	} while(0)


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
 * general stuff for lsr_t *
 */

/*
 * Pointers have 00, while masked types have a 1 in one of the two least
 * significant bits.
 */
static inline bool lsr_is_masked(lsr_t *ptr) {
	return (size_t)ptr & 3;
}

static inline bool lsr_ptr_is_refcounted(lsr_t *ptr) {
	return ptr->type > '\04';
}

static inline char lsr_type(lsr_t *ptr) {
	return ptr->type;
}

/*
 * Initialize the common data for all pointer types (type and ref-counter).
 * Typically done after allocation.
 */
static inline void lsr_init_ptr(lsr_t *ptr, char type) {
	ptr->type = type;
	ptr->refc = 0;
}

/*-------------------
 * Ref-counter stuff
 *-------------------*/

/* ref-counter manipulation */
static inline void lsr_refc_add(lsr_t *ptr, size_t delta) {
	if (lsr_is_masked(ptr) || !lsr_ptr_is_refcounted(ptr))
		return; /* not ref-counted */
	ptr->refc += delta;
}
static inline void lsr_incref(lsr_t *ptr) {
	lsr_refc_add(ptr, 1);
}
static inline void lsr_decref(lsr_t *ptr) {
	lsr_refc_add(ptr, -1);
}
/* ref-counter manipulation */
static inline size_t lsr_get_refc(lsr_t *ptr) {
#ifdef LSR_DEBUG
	if (lsr_is_masked(ptr) || !lsr_ptr_is_refcounted(ptr))
		lsr_type_error(ptr, "lsr_get_refc");
#endif
	return ptr->refc;
}

/*----------------------------------------------------------------------
 * Booleans, stored inside pointers as 01111x10, where x is 1 for true,
 * 0 for false.  In ascii, these are '>' (true) and ':' (false).
 *----------------------------------------------------------------------*/

/*
 * Convert a pointer to boolean.  If it's not a boolean, the result is
 * undefined.  Use lsr_is_boolean if you need to check in advance.
 */
static inline bool lsr_ptr_to_bool(lsr_t * ptr) {
	size_t n = (size_t)ptr;
	return (bool)(n & ~':');
}

static inline lsr_t * lsr_bool_to_ptr(bool yes) {
	return (lsr_t *)(size_t)(yes ? '>' : ':');
}

static inline bool lsr_is_boolean(lsr_t * ptr) {
	return ((size_t)(ptr) & ':') == ':';
}

/*
 * Check if a pointer contains a masked boolean and raise an error if not.
 */
static inline void lsr_ensure_boolean(lsr_t * ptr) {
	if (!lsr_is_boolean(ptr))
		lsr_error("Non-boolean value in lsr_ensure_boolean");
}

/*-------------------------------------------------------------------*/

