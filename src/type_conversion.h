#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

#include <stdint.h>

#include "bit64_conversion.h"
#include "float16_conversion.h"

SEXP type_convert_chunk(SEXP input, SEXP new_type, SEXP _n_bytes, SEXP _is_signed, SEXP Rdim);
SEXP type_convert_INTEGER(void *raw_buffer, R_xlen_t length, int n_bytes, int is_signed);
SEXP type_convert_REAL(void *raw_buffer, R_xlen_t length, int n_bytes);
SEXP type_convert_LOGICAL(void *raw_buffer, R_xlen_t length);
