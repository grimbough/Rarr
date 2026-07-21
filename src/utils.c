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

// This custom functions allows us to initialize an array with a fill value.
// The base array() function either init to NA or will recycle.
// In our case, we know fill_value will always be a single value.
SEXP init_array(SEXP fill_value, SEXP dim) {
  SEXP result = PROTECT(allocArray(TYPEOF(fill_value), dim));
  R_xlen_t n = XLENGTH(result);
  switch (TYPEOF(fill_value)) {
    case INTSXP: {
      int fill = INTEGER(fill_value)[0];
      for (R_xlen_t i = 0; i < n; i++) {
        INTEGER(result)[i] = fill;
      }
      break;
    }
    case RAWSXP: {
      Rbyte fill = RAW(fill_value)[0];
      for (R_xlen_t i = 0; i < n; i++) {
        RAW(result)[i] = fill;
      }
      break;
    }
    case REALSXP: {
      double fill = REAL(fill_value)[0];
      for (R_xlen_t i = 0; i < n; i++) {
        REAL(result)[i] = fill;
      }
      break;
    }
    case LGLSXP: {
      bool fill = LOGICAL(fill_value)[0];
      for (R_xlen_t i = 0; i < n; i++) {
        LOGICAL(result)[i] = fill;
      }
      break;
    }
    case STRSXP: {
      SEXP fill = STRING_ELT(fill_value, 0);
      for (R_xlen_t i = 0; i < n; i++) {
        SET_STRING_ELT(result, i, fill);
      }
      break;
    }
    case VECSXP: {
      SEXP fill = VECTOR_ELT(fill_value, 0);
      for (R_xlen_t i = 0; i < n; i++) {
        SET_VECTOR_ELT(result, i, fill);
      }
      break;
    }
    default:
      error("Unsupported fill value type");
  }
  UNPROTECT(1);
  return result;
}