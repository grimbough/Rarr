#include "decompress.h"

SEXP decompress_chunk_BLOSC(SEXP input) {
  
  void* p_input = RAW(input);
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

/* not required as R has a native decompressor for ZLIB */
// SEXP decompress_chunk_ZLIB(SEXP input, SEXP _outbuffersize) {
//   
//   void* p_input = RAW(input);
//   void *p_output;
// 
//   size_t outbufsize;
//   SEXP output;
// 
//   outbufsize = INTEGER(_outbuffersize)[0];
//   output = PROTECT(allocVector(RAWSXP, outbufsize));
//   p_output = RAW(output);
//   uncompress(p_output, &outbufsize, p_input, xlength(input));
// 
//   UNPROTECT(1);
//   return output;
// } 
