#include <R.h>
#include <Rdefines.h>

#include <zlib.h>
#include "compression_tools/blosc/lib/blosc-1.20.1/blosc.h"

SEXP decompress_chunk(SEXP input, SEXP _decompressor);

