#include "bit64_conversion.h"

void int64_to_int32(void* in_buf, size_t n, void* out_buf, int is_signed) {
  
  R_xlen_t i;
  int8_t warn = 0;
  
  if (is_signed == 1) {
    for (i=0; i<n; i++) {
      ((int32_t *)out_buf)[i] = ((int64_t *)in_buf)[i];
    }
    for (i=0; i<n; i++) {
      if (((int64_t *)in_buf)[i] > INT_MAX) {
        ((int32_t *)out_buf)[i] = INT_MIN;
        warn = 1;
      }
      if (((int64_t *)in_buf)[i] < INT_MIN) {
        ((int32_t *)out_buf)[i] = INT_MIN;
        warn = 1;
      }
    }
  } else {
    for (i=0; i<n; i++) {
      ((int *)out_buf)[i] = ((uint64_t *)in_buf)[i];
    }
    for (i=0; i<n; i++) {
      if (((uint64_t *)in_buf)[i] > INT_MAX) {
        ((int *)out_buf)[i] = INT_MIN;
        warn = 1;
      }
    }
  }
  
  if(warn > 0) {
    warning("NAs produced by integer overflow while converting 64-bit integer from HDF5 to a 32-bit integer in R.\nChoose bit64conversion='bit64' or bit64conversion='double' to avoid data loss");
  }
  
}
