#ifndef _RTL_INSIDE_RT_LISP_H_
# error "rtl/rto.h should only be included indirectly via rt-lisp.h"
#endif

typedef struct rtl_RTOChunkHeader {
  char type[4];

  // Unique identifier for this chunk, within this .rto file.
  uint32_t id;

  // The number of bytes in the chunk (not including the header).
  uint32_t size;
} rtl_RTOChunkHeader;

typedef struct rtl_RTOFile {
  uint32_t nbrChunks;
  uint32_t totalSize;

  // The first nbrChunks words of data are the offsets of each
  // rtl_RTOChunkHeader.
  uint32_t data[];
} rtl_RTOFile;

// One big buffer full of code.
#define RTO_CHUNK_TYPE_CODE { 'C', 'O', 'D', 'E' }

typedef struct rtl_RTOCodeChunk {
  rtl_RTOChunkHeader hdr;

  // The number of rtl_Words of code.
  uint32_t len;

  // The code data itself.
  rtl_Word code[];
} rtl_RTOCodeChunk;

// A list of the symbols defined by this object file.
#define RTO_CHUNK_TYPE_SYMS { 'S', 'Y', 'M', 'S' }

// A symbol definition.
typedef struct rtl_RTOSymDef {
  uint32_t pkg;
  uint32_t sym;

  // An offset into this RTO's code segment.
  uint32_t addr;
} rtl_RTOSymDef;

typedef struct rtl_RTOSymsChunk {
  rtl_RTOChunkHeader hdr;

  // The number of symbol definitions in this chunk.
  uint32_t len;

  rtl_RTOSymDef defs[];
} rtl_RTOSymsChunk;

// A list of symbols which this file references, and the places where they're
// referenced.
#define RTO_CHUNK_TYPE_REFS { 'R', 'E', 'F', 'S' }

typedef struct rtl_RTORef {
  uint32_t pkg;
  uint32_t sym;

  // The offset into the code chunk where this reference occurs. This is the
  // offset of a word which should be replaced by the address this symbol refers
  // to.
  uint32_t loc;
} rtl_RTORef;
