#include "rt-lisp.h"

#include <stddef.h>

#define container_of(PTR, CONTAINER, FIELD) ({				\
      typeof(PTR) ___self___ = (PTR);					\
      CONTAINER *___super___ = NULL;					\
      if (___self___) {							\
	___super___ = ((CONTAINER *)((char *)___self___ -		\
				     offsetof(CONTAINER, FIELD)));	\
      }									\
      ___super___;							\
    })									\
  // end of multi-line macro


