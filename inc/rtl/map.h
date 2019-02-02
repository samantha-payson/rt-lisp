#ifndef _RTL_INSIDE_RT_LISP_H_
# error "rtl/map.h should only be included indirectly via rt-lisp.h"
#endif

static inline
int rtl_isMap(rtl_Word w) { return rtl_typeOf(w) == RTL_MAP; }


static inline
rtl_Word rtl_emptyMap() { return RTL_MAP; }

static inline
bool rtl_isEmptyMap(rtl_Word w) { return w == RTL_MAP; }

rtl_Word rtl_mapInsert(rtl_Machine *M,
		       rtl_Word    map,
		       rtl_Word    key,
		       rtl_Word    val);

rtl_Word rtl_mapLookup(rtl_Machine *M, rtl_Word map, rtl_Word key);

rtl_Word rtl_mapDelete(rtl_Machine *M, rtl_Word map, rtl_Word key);
