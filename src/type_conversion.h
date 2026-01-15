#include "Rarr.h"

#include "bit64_conversion.h"
#include "float16_conversion.h"

SEXP type_convert_int(SEXP input, SEXP _n_bytes);
SEXP type_convert_uint(SEXP input, SEXP _n_bytes);
SEXP type_convert_float(SEXP input, SEXP _n_bytes);
SEXP type_convert_bool(SEXP input, SEXP _n_bytes);
SEXP type_convert_string(SEXP input, SEXP _n_bytes);
