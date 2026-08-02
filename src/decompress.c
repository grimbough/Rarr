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


/*! ZSTD_decompress() :
 *  `compressedSize` : must be the _exact_ size of some number of compressed and/or skippable frames.
 *  `dstCapacity` is an upper bound of originalSize to regenerate.
 *  If user cannot imply a maximum upper bound, it's better to use streaming mode to decompress data.
 *  @return : the number of bytes decompressed into `dst` (<= `dstCapacity`),
 *            or an errorCode if it fails (which can be tested using ZSTD_isError()). 
ZSTDLIB_API size_t ZSTD_decompress( void* dst, size_t dstCapacity,
                                    const void* src, size_t compressedSize); */

SEXP decompress_chunk_ZSTD(SEXP input) {
  
  const void* p_input = RAW(input);
  void* p_output;
  const size_t compressed_size = (size_t) xlength(input);
  SEXP output;
  size_t dsize;

  const unsigned long long frameSize = ZSTD_getFrameContentSize(p_input, compressed_size);
  if (frameSize == ZSTD_CONTENTSIZE_UNKNOWN || frameSize == ZSTD_CONTENTSIZE_ERROR) {
    // FIXME: When ZSTD_CONTENTSIZE_UNKNOWN, we can still use streaming mode according to
    // the docs. 
    error("Unable to determine decompressed buffer size for zstd frame; ensure metadata provides nbytes\n");
  }
  /* Ensure frameSize fits in size_t on this platform. 
  FIXME: we should implement streaming decompression mode in thisn case, as recommended
  in the ZSTD docs. */
  if (frameSize > SIZE_MAX) {
    error("decompressed frame size (%llu) exceeds platform maximum (%zu); use streaming decompression\n",
          frameSize, SIZE_MAX);
  }

  output = PROTECT(Rf_allocVector(RAWSXP, (R_xlen_t) frameSize));
  p_output = RAW(output);

  dsize = ZSTD_decompress(p_output, (size_t) frameSize, p_input, compressed_size);
  if (ZSTD_isError(dsize)) {
    error("zstd decompression error - error code: %zu (%s)\n", dsize, ZSTD_getErrorName(dsize));
  }

  /* set the length of our output vector the actual number of decompressed
   * bytes. _outbuffersize/frameSize is an upper bound. */
  output = Rf_xlengthgets(output, dsize);

  UNPROTECT(1);
  return output;
}
