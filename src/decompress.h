#include <R.h>
#include <Rdefines.h>

//#include <zlib.h>
#include "compression_tools/blosc/lib/blosc-1.20.1/blosc.h"

//SEXP decompress_chunk(SEXP input, SEXP _decompressor, SEXP _outbuffersize);
SEXP decompress_chunk_BLOSC(SEXP input);
//SEXP decompress_chunk_ZLIB(SEXP input, SEXP _outbuffersize);

