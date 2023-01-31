#include "Rarr.h"

#ifndef _COMPRESSION_H
  #define _COMPRESSION_H
  #include "compression_tools/blosc/lib/blosc-1.20.1/blosc.h"
  #include "compression_tools/blosc/lib/lz4-1.9.2/lz4.h"
  #include "compression_tools/blosc/lib/lz4-1.9.2/lz4hc.h"
#endif

SEXP compress_chunk_BLOSC(SEXP input);
