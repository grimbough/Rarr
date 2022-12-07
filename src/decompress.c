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

#include "decompress.h"


#define ZLIB  0
#define BLOSC 1

SEXP decompress_chunk(SEXP input, SEXP _decompressor) {
  
  void* p_input = RAW(input);
  void *p_output;
  size_t cbytes, blocksize, outbuf_size;
  int decompressor = INTEGER(_decompressor)[0];
  SEXP output;
  
  switch(decompressor) {
  case ZLIB:
    
  case BLOSC:
    blosc_cbuffer_sizes(p_input, &outbuf_size, &cbytes, &blocksize);
    output = PROTECT(allocVector(RAWSXP, outbuf_size));
    p_output = RAW(output);
    blosc_decompress(p_input, p_output, outbuf_size);
    break;
  default:
    error("Unkown compress algorithm");
    break;
  }
  
  UNPROTECT(1);
  return output;
} 
