#include "Rarr.h"
#include "decompress.h"
#include "compress.h"
#include "codec_vlen-utf8.h"
#include "utils.h"

static const R_CallMethodDef callMethods[] = {
  {"decompress_chunk_BLOSC", (DL_FUNC) &decompress_chunk_BLOSC, 1},
  {"decompress_chunk_LZ4", (DL_FUNC) &decompress_chunk_LZ4, 2},
  {"decompress_chunk_ZSTD", (DL_FUNC) &decompress_chunk_ZSTD, 1},
  
  {"compress_chunk_BLOSC", (DL_FUNC) &compress_chunk_BLOSC, 6},
  {"compress_chunk_LZ4", (DL_FUNC) &compress_chunk_LZ4, 1},
  {"compress_chunk_ZSTD", (DL_FUNC) &compress_chunk_ZSTD, 2},

  {"codec_vlen_utf8_decode_c", (DL_FUNC) &codec_vlen_utf8_decode_c, 2},

  {"is_compact", (DL_FUNC) &is_compact, 1},
  {NULL, NULL, 0}
};

void R_init_Rarr(DllInfo *info)
{
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(info, TRUE);
}
