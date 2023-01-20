#include <Rdefines.h>
#include "type_conversion.h"
#include "decompress.h"

R_CallMethodDef callMethods[] = {
  {"decompress_chunk_BLOSC", (DL_FUNC) &decompress_chunk_BLOSC, 1},
//  {"decompress_chunk_ZLIB", (DL_FUNC) &decompress_chunk_ZLIB, 2},
  {"type_convert_chunk", (DL_FUNC) &type_convert_chunk, 5},
  {NULL, NULL, 0}
};

void R_init_Rarr(DllInfo *info)
{
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(info, TRUE);
}
