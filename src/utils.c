#include "utils.h"

SEXP is_compact(SEXP x) {
  if (ALTREP(x)) {
    return ScalarLogical(TRUE);
  }
  return ScalarLogical(FALSE);
}