/*
 * array.h: Dynamic array implemented as an array deque, circular buffer.
 */

typedef struct {
	LSR_HEADER;        /* type = LSR_ARRAY */
	unsigned int cap;  /* capacity, actual length of the els array */
	unsigned int off;  /* offset to the first element in els */
	unsigned int len;  /* length */
	lsr_t *els[1];     /* elements */
} lsr_array_t;

#define LSR_ARRAY_MIN_CAPACITY 4

static inline size_t lsr_sizeof_array(unsigned int cap) {
	return sizeof(lsr_array_t) + (cap - 1) * sizeof(lsr_t *);
}

/*
 * Convert external index to internal one. (Used internally.)
 *
 * i % cap == i & (cap - 1), since cap always is a power of 2.
 */
static inline unsigned int lsr_array_idx(lsr_array_t *a, unsigned int i) {
	return (a->off + i) & (a->cap - 1);
}

static inline lsr_t *lsr_array_create(unsigned int cap) {
	lsr_array_t *arr = (lsr_array_t *)lsr_alloc(lsr_sizeof_array(cap));
	lsr_init_ptr((lsr_t *)arr, LSR_ARRAY);
	arr->len = 0;
	arr->off = 0;
	arr->cap = cap;
	return (lsr_t *)arr;
}

static inline lsr_t *lsr_array_get(lsr_t *array, unsigned int index) {
	lsr_array_t *a = (lsr_array_t *)array;
	unsigned int pos = lsr_array_idx(a, index);
	return a->els[pos];
}

/* Reserve space for at least n more elements */
static inline lsr_array_t *lsr_array_reserve(lsr_array_t *a, unsigned int n) {
	if (a->cap < a->len + n) {
		/* calulate and set new capacity */
		unsigned int oldcap = a->cap;
		do {
			a->cap = a->cap >= LSR_ARRAY_MIN_CAPACITY ? a->cap << 1
			                                          : LSR_ARRAY_MIN_CAPACITY;
		} while (a->len + n > a->cap);
		/* allocate more mem */
		a = (lsr_array_t *)lsr_realloc(a,
		                               lsr_sizeof_array(a->cap),
		                               lsr_sizeof_array(oldcap));
		/* adjust content to the increased capacity */
		if (a->off + a->len > oldcap) {
			/* it warps around. make it warp around the new boundary. */
			memmove(&(a->els[a->off + a->cap - oldcap]),
			        &(a->els[a->off]),
			        sizeof(lsr_t *) * (a->cap - oldcap));
			memset(&(a->els[a->off]), 0,
			       sizeof(lsr_t *) * (a->cap - oldcap));
			a->off += a->cap - oldcap;
		}
	}
	return a;
}

/*
 * Reduces the capacity somewhat if less than 25% full
 */
static inline lsr_array_t *lsr_array_reduce_size(lsr_array_t *a) {
	if (a->len << 2 <= a->cap && a->cap > LSR_ARRAY_MIN_CAPACITY) {
		/* it is down to 1/4. reduce cap to len * 2 so it is half full */
		unsigned int oldcap = a->cap;
		/* calulate and set new capacity */
		do {
			a->cap = a->cap >> 1;
		} while (a->len << 2 <= a->cap && a->cap > LSR_ARRAY_MIN_CAPACITY);
		/* adjust content to decreased capacity */
		if (a->off >= a->cap) {
			/* the whole content is outside, but in one piece */
			memcpy(&(a->els[0]),
			       &(a->els[a->off]),
			       sizeof(lsr_t *) * a->len);
			a->off = 0;
		}
		else if (a->off + a->len >= oldcap) {
			/* it warpped around already. adjust to new boundary. */
			memmove(&(a->els[a->off + a->cap - oldcap]),
			        &(a->els[a->off]),
			        sizeof(lsr_t *) * (a->cap - oldcap));
			a->off += a->cap - oldcap;
		}
		else if (a->off + a->len > a->cap) {
			/* it overflows the new cap. make it warp. */
			memcpy(&(a->els[0]),
			       &(a->els[a->cap]),
			       sizeof(lsr_t *) * (a->off + a->len - a->cap));
		}
		/* free the unused part */
		a = (lsr_array_t *)lsr_realloc(a,
		                               lsr_sizeof_array(a->cap),
		                               lsr_sizeof_array(oldcap));
	}
	return a;
}

/*
 * Returns the number of elements in the array.
 */
static inline unsigned int lsr_array_len(lsr_t *array) {
	return ((lsr_array_t *)array)->len;
}

/*
 * Insert an element at the beginning.
 * May change array ptr if it needs to be reallocated.
 */
static inline void lsr_array_unshift(lsr_t **array, lsr_t *value) {
	lsr_array_t *a = (lsr_array_t *)*array;
	a = lsr_array_reserve(a, 1);
	a->off = lsr_array_idx(a, a->cap - 1);
	a->els[a->off] = value;
	a->len++;
	array = (lsr_t **)&a;
}

/*
 * Remove an element at the beginning and return its value.
 * May change array ptr if it needs to be reallocated.
*/
static inline lsr_t *lsr_array_shift(lsr_t **array) {
	lsr_array_t *a = (lsr_array_t *)*array;
	lsr_t *value = a->els[a->off];
	a->off = lsr_array_idx(a, 1);
	a->len--;
	a = lsr_array_reduce_size(a);
	array = (lsr_t **)&a;
	return value;
}

/*
 * Insert an element at the end. May change array ptr if it needs
 * to be reallocated.
 */
static inline void lsr_array_push(lsr_t **array, lsr_t *value) {
	lsr_array_t *a = (lsr_array_t *)*array;
	a = lsr_array_reserve(a, 1);
	a->els[lsr_array_idx(a, a->len++)] = value;
	array = (lsr_t **)&a;
}

/*
 * Remove an element at the end and return its value, O(1)
 * May change array ptr if it needs to be reallocated.
 */
static inline lsr_t *lsr_array_pop(lsr_t **array) {
	lsr_array_t *a = (lsr_array_t *)*array;
	lsr_t *value = a->els[lsr_array_idx(a, --a->len)];
	a = lsr_array_reduce_size(a);
	array = (lsr_t **)&a;
	return value;
}
