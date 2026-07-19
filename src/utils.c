#include "utils.h"

SEXP is_compact(SEXP x) {
  if (ALTREP(x)) {
    return ScalarLogical(TRUE);
  }
  return ScalarLogical(FALSE);
}

SEXP chop_vec(SEXP x, SEXP _size) {
  R_xlen_t n = XLENGTH(x);
  int size = INTEGER(_size)[0];
  R_xlen_t m = ceil((double)n / size);

  SEXP result = PROTECT(allocVector(VECSXP, m));
  SEXP el;
  
  R_xlen_t start = 0;

  for (R_xlen_t i = 0; i < m; i++) {
    if (start + size > n) {
      size = n - start;
    }
    el = allocVector(TYPEOF(x), size);
    // TODO: figure out how to use GET_REGION_PTR macro
    if (TYPEOF(x) == INTSXP) {
      INTEGER_GET_REGION(x, start, size, INTEGER(el));
    } else if (TYPEOF(x) == RAWSXP) {
      RAW_GET_REGION(x, start, size, RAW(el));
    }
    SET_VECTOR_ELT(result, i, el);
    start += size;
  }
  UNPROTECT(1);
  return result;
}