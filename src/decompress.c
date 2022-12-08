

#include "decompress.h"

/*
#define ZLIB  0
#define BLOSC 1

SEXP decompress_chunk(SEXP input, SEXP _decompressor, SEXP _outbuffersize) {
  
  void* p_input = RAW(input);
  void *p_output;
  size_t cbytes, blocksize, outbuf_size;
  int *p_outbufsize;
  int decompressor = INTEGER(_decompressor)[0];
  SEXP output;
  
  switch(decompressor) {
  case ZLIB:
    p_outbufsize = INTEGER(_outbuffersize);
    output = PROTECT(allocVector(RAWSXP, *p_outbufsize));
    p_output = RAW(output);
    uncompress(p_output, p_outbufsize, p_input, xlength(input));
  case BLOSC:
    blosc_cbuffer_sizes(p_input, &outbuf_size, &cbytes, &blocksize);
    output = PROTECT(allocVector(RAWSXP, outbuf_size));
    p_output = RAW(output);
    blosc_decompress(p_input, p_output, outbuf_size);
    break;
  default:
    error("Unkown compression algorithm");
    break;
  }
  
  UNPROTECT(1);
  return output;
} */

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


/*ZEXTERN int ZEXPORT uncompress OF((Bytef *dest,   uLongf *destLen,
 const Bytef *source, uLong sourceLen));
 
 Decompresses the source buffer into the destination buffer.  sourceLen is
 the byte length of the source buffer.  Upon entry, destLen is the total size
 of the destination buffer, which must be large enough to hold the entire
 uncompressed data.  (The size of the uncompressed data must have been saved
 previously by the compressor and transmitted to the decompressor by some
 mechanism outside the scope of this compression library.) Upon exit, destLen
 is the actual size of the uncompressed data.
 uncompress returns Z_OK if success, Z_MEM_ERROR if there was not
 enough memory, Z_BUF_ERROR if there was not enough room in the output
 buffer, or Z_DATA_ERROR if the input data was corrupted or incomplete.  In
 the case where there is not enough room, uncompress() will fill the output
 buffer with the uncompressed data up to that point.
 */

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
