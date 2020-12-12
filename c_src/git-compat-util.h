/* Slimmed down git utils used by ewah.
 *
 */
#ifndef GIT_COMPAT_UTIL_H
#define GIT_COMPAT_UTIL_H

#include <inttypes.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdint.h>
#include <limits.h>
#include <stdio.h>

# define xmalloc(size)           (malloc(size))
# define xcalloc(nitems, size)   (calloc(nitems, size))

#ifdef HAVE_ALLOCA_H
# include <alloca.h>
# define xalloca(size)      (alloca(size))
# define xalloca_free(p)    do {} while (0)
#else
# define xalloca(size)      (xmalloc(size))
# define xalloca_free(p)    (free(p))
#endif

#define bitsizeof(x)  (CHAR_BIT * sizeof(x))

#define DIV_ROUND_UP(n,d) (((n) + (d) - 1) / (d))

#define maximum_unsigned_value_of_type(a) \
   (UINTMAX_MAX >> (bitsizeof(uintmax_t) - bitsizeof(a)))

#define unsigned_mult_overflows(a, b) \
   ((a) && (b) > maximum_unsigned_value_of_type(a) / (a))

#define ALLOC_ARRAY(x, alloc) (x) = malloc(sizeof(*(x)) * (alloc))
#define CALLOC_ARRAY(x, alloc) (x) = calloc((alloc), sizeof(*(x)));
#define REALLOC_ARRAY(x, alloc) (x) = realloc((x), sizeof(*(x)) * (alloc))

// From git cache.h
#define alloc_nr(x) (((x)+16)*3/2)

#define ALLOC_GROW(x, nr, alloc) \
   do { \
      if ((nr) > alloc) { \
         if (alloc_nr(alloc) < (nr)) \
         alloc = (nr); \
         else \
         alloc = alloc_nr(alloc); \
         REALLOC_ARRAY(x, alloc); \
      } \
   } while (0)

#endif
