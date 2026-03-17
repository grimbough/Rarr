#include "bit64_conversion.h"


void uint32_to_int32(const void* in_buf, size_t n, void* out_buf) {
  
  R_xlen_t i;
  
  for (i = 0; i < n; i++) {
    if (((uint32_t *)in_buf)[i] > INT_MAX) {
      ((int32_t *)out_buf)[i] = INT_MIN;
      Rf_warning("Integer overflow: converting 32bit unsigned integer to 32bit signed integer resulted in NA values");
    } else {
      ((int32_t *)out_buf)[i] = ((uint32_t *)in_buf)[i];
    }
  }
  
}

void int64_to_int32(const void* in_buf, size_t n, void* out_buf, bool is_signed) {
  
  R_xlen_t i;
  
  if (is_signed) {
    for (i=0; i<n; i++) {
      if (((int64_t *)in_buf)[i] > INT_MAX) {
        ((int32_t *)out_buf)[i] = INT_MIN;
        Rf_warning("Integer overflow: converting 64bit integer to 32bit integer resulted in NA values");
      }
      else if (((int64_t *)in_buf)[i] < INT_MIN) {
        ((int32_t *)out_buf)[i] = INT_MIN;
        Rf_warning("Integer underflow: converting 64bit integer to 32bit integer resulted in NA values");
      } else {
        ((int32_t *)out_buf)[i] = ((int64_t *)in_buf)[i];
      }
    }
  } else {
    for (i=0; i<n; i++) {
      if (((uint64_t *)in_buf)[i] > INT_MAX) {
        ((int *)out_buf)[i] = INT_MIN;
        Rf_warning("Integer overflow: converting 64bit integer to 32bit integer resulted in NA values");
      } else {
        ((int *)out_buf)[i] = ((uint64_t *)in_buf)[i];
      }
    }
  }

}
