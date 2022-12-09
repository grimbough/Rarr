#include "decompress.h"

SEXP decompress_chunk_BLOSC(SEXP input) {
  
  void* p_input = RAW(input);
  void *p_output;
  size_t cbytes, blocksize, outbuf_size;
  SEXP output;
  
    blosc_cbuffer_sizes(p_input, &outbuf_size, &cbytes, &blocksize);
    output = PROTECT(allocVector(RAWSXP, outbuf_size));
    p_output = RAW(output);
    blosc_decompress(p_input, p_output, outbuf_size);

  UNPROTECT(1);
  return output;
} 


SEXP decompress_chunk_ZLIB(SEXP input, SEXP _outbuffersize) {
  
  void* p_input = RAW(input);
  void *p_output;

  size_t outbufsize;
  SEXP output;

  outbufsize = INTEGER(_outbuffersize)[0];
  output = PROTECT(allocVector(RAWSXP, outbufsize));
  p_output = RAW(output);
  uncompress(p_output, &outbufsize, p_input, xlength(input));

  UNPROTECT(1);
  return output;
} 
