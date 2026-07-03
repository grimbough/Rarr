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
  
  SEXP output = PROTECT(R_allocResizableVector(RAWSXP, xlength(input)+BLOSC_MAX_OVERHEAD));
  void *p_output = RAW(output);

  blosc_init();
  blosc_set_compressor(compressor_name);
  blosc_set_blocksize(block_size);
  int dsize = blosc_compress(
    compression_level, 
    shuffle_mode, 
    typesize,
    xlength(input), 
    p_input, 
    p_output, 
    xlength(output)
  );

  if(dsize > 0) {
    /* shrink our output buffer to contain only the compressed bytes */
    R_resizeVector(output, dsize);
    UNPROTECT(1);
    return output;
  }
  if(dsize == 0) {
    /* if compression results in a bigger chunk, just use the original input */
    UNPROTECT(1);
    return input;
  }

  /* something terrible happened */
  error("BLOSC compression error - error code: %d\n", dsize);
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

SEXP compress_chunk_ZSTD(SEXP input, SEXP compression_level) {
  
  /*! ZSTD_compress() :
   *  Compresses `src` content as a single zstd compressed frame into already allocated `dst`.
   *  NOTE: Providing `dstCapacity >= ZSTD_compressBound(srcSize)` guarantees that zstd will have
   *        enough space to successfully compress the data.
   *  @return : compressed size written into `dst` (<= `dstCapacity),
   *            or an error code if it fails (which can be tested using ZSTD_isError()). 
  ZSTDLIB_API size_t ZSTD_compress( void* dst, size_t dstCapacity,
                                    const void* src, size_t srcSize,
                                    int compressionLevel); */
  
  const void* p_input = RAW(input);
  void* p_output; 
  const size_t input_size = (size_t) xlength(input);
  const size_t output_size = (size_t) ZSTD_compressBound(input_size);
  const int compressionLevel = INTEGER(compression_level)[0];
  
  SEXP output = PROTECT(R_allocResizableVector(RAWSXP, output_size));
  p_output = RAW(output);
  
  int dsize = ZSTD_compress(p_output, output_size, p_input, input_size, compressionLevel);
  
  if(ZSTD_isError(dsize)) {
    error("zstd decompression error - error code: %d\n", dsize);
  }
  
  /* shrink our output vector to include only the compressed bytes */
  R_resizeVector(output, dsize);
  
  UNPROTECT(1);
  return output;
} 

/* From https://cran.r-project.org/doc/manuals/r-devel/R-exts.html#Some-backports-1 */
#if R_VERSION < R_Version(4, 6, 0)
SEXP R_allocResizableVector(SEXPTYPE type, R_xlen_t maxlen) {
  SEXP ret = Rf_allocVector(type, maxlen);
  SET_TRUELENGTH(ret, maxlen);
  SET_GROWABLE_BIT(ret);
  return ret;
}
#endif
