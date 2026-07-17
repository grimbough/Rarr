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
  R_xlen_t m = n / size;

  SEXP result = PROTECT(allocVector(VECSXP, m));
  SEXP el;

  for (R_xlen_t i = 0; i < m; i++) {
    el = allocVector(TYPEOF(x), size);
    memcpy(INTEGER(el), INTEGER(x) + i * size, size * sizeof(int));
    SET_VECTOR_ELT(result, i, el);
  }
  UNPROTECT(1);
  return result;
}