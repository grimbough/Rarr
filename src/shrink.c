#include "compress.h"

SEXP compress_chunk_BLOSC(SEXP input) {
  
  void* p_input = RAW(input);
  void *p_output;
  SEXP output;
  int dsize;
  
  output = PROTECT(allocVector(RAWSXP, LENGTH(input)+BLOSC_MAX_OVERHEAD));
  p_output = RAW(output);

  dsize = blosc_compress_ctx(5, BLOSC_SHUFFLE, 1, LENGTH(input), p_input, p_output, LENGTH(input)+BLOSC_MAX_OVERHEAD, "lz4", 0, 1);
    
  if(dsize == 0) {
    /* if compression results in a bigger chunk, just use the original input */
    p_output = p_input;
  }  
  if(dsize < 0) {
    error("BLOSC compression error - error code: %d\n", dsize);
  }

  UNPROTECT(1);
  return output;
} 
 /*
SEXP compress_chunk_LZ4(SEXP input, SEXP _outbuffersize) {
  
  void* p_input = (void *)RAW(input);
  void* p_output;
  int outbuf_size;
  int compressed_size = (int) xlength(input);
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
*/ 
 
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
