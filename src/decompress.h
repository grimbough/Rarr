#include "Rarr.h"

#ifndef _COMPRESSION_H
  #define _COMPRESSION_H
  #include "compression_tools/blosc/lib/blosc-1.21.6/blosc.h"
  #include "compression_tools/blosc/lib/lz4-1.9.2/lz4.h"
  #include "compression_tools/blosc/lib/lz4-1.9.2/lz4hc.h"
  #include "compression_tools/zstd/zstd.h"
#endif

SEXP decompress_chunk_BLOSC(SEXP input);
//SEXP decompress_chunk_ZLIB(SEXP input, SEXP _outbuffersize);
SEXP decompress_chunk_LZ4(SEXP input, SEXP _outbuffersize);
SEXP decompress_chunk_ZSTD(SEXP input, SEXP _outbuffersize);
