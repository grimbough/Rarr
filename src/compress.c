#include "compress.h"

SEXP compress_chunk_BLOSC(
  SEXP input,
  SEXP type_size,
  SEXP cname,
  SEXP clevel,
  SEXP shuffle,
  SEXP blocksize
) {
  
  const void* p_input = RAW(input);

  const char *compressor_name = CHAR(STRING_ELT(cname, 0));
  const int compression_level = INTEGER(clevel)[0];
  const int shuffle_mode = INTEGER(shuffle)[0];
  const size_t typesize = (size_t)INTEGER(type_size)[0];
  const size_t block_size = (size_t)INTEGER(blocksize)[0];
  
  SEXP output = PROTECT(R_allocResizableVector(RAWSXP, LENGTH(input)+BLOSC_MAX_OVERHEAD));
  void *p_output = RAW(output);

  blosc_init();
  blosc_set_compressor(compressor_name);
  blosc_set_blocksize(block_size);
  int dsize = blosc_compress(
    compression_level, 
    shuffle_mode, 
    typesize,
    LENGTH(input), 
    p_input, 
    p_output, 
    LENGTH(output)
  );

  if(dsize > 0) {
    /* shrink our output buffer to contain only the compressed bytes */
    R_resizeVector(output, dsize);
  } else if(dsize == 0) {
    /* if compression results in a bigger chunk, just use the original input */
    p_output = (void *)p_input;
  }  else {
    /* something terrible happened */
    error("BLOSC compression error - error code: %d\n", dsize);
  }

  UNPROTECT(1);
  return output;
} 

SEXP compress_chunk_LZ4(SEXP input) {
  
  const void* p_input = RAW(input);
  void* p_output; 
  const int input_size = (int) xlength(input);
  const int output_size = LZ4_compressBound(input_size);
  SEXP output;
  int dsize;
  
  output = PROTECT(R_allocResizableVector(RAWSXP, output_size));
  p_output = RAW(output);

  dsize = LZ4_compress_default((char *)p_input, (char *)p_output, input_size, output_size);
  
  if(dsize <= 0) {
    error("LZ4 decompression error - error code: %d\n", dsize);
  }
  
  /* shrink our output vector to include only the compressed bytes */
  R_resizeVector(output, dsize);

  UNPROTECT(1);
  return output;
} 
