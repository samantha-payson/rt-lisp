// This file is part of RT Lisp.
//
// RT Lisp is free software: you can redistribute it and/or modify it under the
// terms of the GNU Lesser General Public License as published by the Free
// Software Foundation, either version 3 of the License, or (at your option) any
// later version.
//
// RT Lisp is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
// A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more
// details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with RT Lisp.  If not, see <https://www.gnu.org/licenses/>.

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


