#include "Rarr.h"

#ifndef _COMPRESSION_H
  #define _COMPRESSION_H
  #include "compression_tools/blosc/lib/blosc-1.21.6/blosc.h"
  #include "compression_tools/blosc/lib/lz4-1.10.0/lz4.h"
  #include "compression_tools/blosc/lib/lz4-1.10.0/lz4hc.h"
  #include "compression_tools/zstd-1.5.7/zstd.h"
#endif

SEXP compress_chunk_BLOSC(
  SEXP input,
  SEXP type_size,
  SEXP cname,
  SEXP clevel,
  SEXP shuffle,
  SEXP blocksize
);
SEXP compress_chunk_LZ4(SEXP input);
SEXP compress_chunk_ZSTD(SEXP input, SEXP compression_level);
