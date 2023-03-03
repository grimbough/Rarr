#include "compress.h"

SEXP compress_chunk_BLOSC(SEXP input, SEXP type_size) {
  
  void* p_input = RAW(input);
  void *p_output;
  SEXP output;
  int dsize;
  int clevel = 5;
  size_t typesize = (size_t)INTEGER(type_size)[0];
  
  output = PROTECT(allocVector(RAWSXP, LENGTH(input)+BLOSC_MAX_OVERHEAD));
  p_output = RAW(output);

  blosc_init();
  blosc_set_compressor("lz4");
  dsize = blosc_compress(clevel, BLOSC_SHUFFLE, typesize, LENGTH(input), 
                         p_input, p_output, LENGTH(output));

  if(dsize > 0) {
    /* shrink our output buffer to contain only the compressed bytes */
    SETLENGTH(output, dsize);
  } else if(dsize == 0) {
    /* if compression results in a bigger chunk, just use the original input */
    p_output = p_input;
  }  else {
    /* something terrible happened */
    error("BLOSC compression error - error code: %d\n", dsize);
  }

  UNPROTECT(1);
  return output;
} 

SEXP compress_chunk_LZ4(SEXP input) {
  
  void* p_input = (void *)RAW(input);
  void* p_output; 
  int input_size = (int) xlength(input);
  int output_size = LZ4_compressBound(input_size);
  SEXP output;
  int dsize;
  
  output = PROTECT(allocVector(RAWSXP, output_size));
  p_output = RAW(output);

  dsize = LZ4_compress_default((char *)p_input, (char *)p_output, input_size, output_size);
  
  if(dsize <= 0) {
    error("LZ4 decompression error - error code: %d\n", dsize);
  }
  
  /* shrink our output vector to include only the compressed bytes */
  SETLENGTH(output, dsize);

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
