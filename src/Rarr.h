#ifndef _RARR_H
#define _RARR_H
  
#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <Rversion.h> 
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
  
/* From https://cran.r-project.org/doc/manuals/r-devel/R-exts.html#Some-backports-1 */
#if R_VERSION < R_Version(4, 6, 0)
SEXP R_allocResizableVector(SEXPTYPE type, R_xlen_t maxlen);
#define R_resizeVector SET_LENGTH
#endif

#endif
