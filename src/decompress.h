#include <R.h>
#include <Rdefines.h>

//#include <zlib.h>
#include "compression_tools/blosc/lib/blosc-1.20.1/blosc.h"
#include "compression_tools/blosc/lib/lz4-1.9.2/lz4.h"
#include "compression_tools/blosc/lib/lz4-1.9.2/lz4hc.h"

#define LZ4_DISABLE_DEPRECATE_WARNINGS

//SEXP decompress_chunk(SEXP input, SEXP _decompressor, SEXP _outbuffersize);
SEXP decompress_chunk_BLOSC(SEXP input);
//SEXP decompress_chunk_ZLIB(SEXP input, SEXP _outbuffersize);
SEXP decompress_chunk_LZ4(SEXP input, SEXP _outbuffersize);
