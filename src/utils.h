#include "Rarr.h"
#include <R_ext/Altrep.h>

SEXP is_compact(SEXP x);
SEXP chop_vec(SEXP x, SEXP _sizes);
SEXP init_array(SEXP fill_value, SEXP dim);
