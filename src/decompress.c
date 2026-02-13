#include "decompress.h"
#include <limits.h>

SEXP decompress_chunk_BLOSC(SEXP input) {
  
  const void* p_input = RAW(input);
  void *p_output;
  size_t cbytes, blocksize, outbuf_size;
  SEXP output;
  int dsize;
  
  blosc_cbuffer_sizes(p_input, &outbuf_size, &cbytes, &blocksize);
  output = PROTECT(allocVector(RAWSXP, outbuf_size));
  p_output = RAW(output);
  dsize = blosc_decompress_ctx(p_input, p_output, outbuf_size, 1);
  if(dsize < 0) {
    error("BLOSC decompression error - error code: %d\n", dsize);
  }

  UNPROTECT(1);
  return output;
} 

SEXP decompress_chunk_LZ4(SEXP input, SEXP _outbuffersize) {
  
  const void* p_input = RAW(input);
  void* p_output;
  int outbuf_size;
  const int compressed_size = (int) xlength(input);
  SEXP output;
  int dsize;
  
  outbuf_size = INTEGER(_outbuffersize)[0];
  output = PROTECT(allocVector(RAWSXP, outbuf_size));
  p_output = RAW(output);

  dsize = LZ4_decompress_safe((char *)p_input, (char *)p_output, compressed_size, outbuf_size);
  if(dsize < 0) {
    error("LZ4 decompression error - error code: %d\n", dsize);
  }

  UNPROTECT(1);
  return output;
} 
