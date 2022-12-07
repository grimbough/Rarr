#include <Rdefines.h>
#include "read_chunk.h"

R_CallMethodDef callMethods[] = {
  {"decompress_chunk", (DL_FUNC) &decompress_chunk, 1},
  {"type_convert_chunk", (DL_FUNC) &type_convert_chunk, 5},
  {NULL, NULL, 0}
};

void R_init_Rarr(DllInfo *info)
{
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(info, TRUE);
}
