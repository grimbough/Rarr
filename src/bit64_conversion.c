#include "bit64_conversion.h"


int32_t uint32_to_int32(void* in_buf, size_t n, void* out_buf) {
  
  R_xlen_t i;
  int32_t raise_warning = 0;
  
  for (i = 0; i < n; i++) {
    ((int32_t *)out_buf)[i] = ((uint32_t *)in_buf)[i];
  }
  for (i = 0; i < n; i++) {
    if (((uint32_t *)in_buf)[i] > INT_MAX) {
      ((int32_t *)out_buf)[i] = INT_MIN;
      raise_warning = 1;
    }
  }
  
  return(raise_warning);
  
}

int8_t int64_to_int32(void* in_buf, size_t n, void* out_buf, int is_signed) {
  
  R_xlen_t i;
  int8_t raise_warning = 0;
  
  if (is_signed == 1) {
    for (i=0; i<n; i++) {
      ((int32_t *)out_buf)[i] = ((int64_t *)in_buf)[i];
    }
    for (i=0; i<n; i++) {
      if (((int64_t *)in_buf)[i] > INT_MAX) {
        ((int32_t *)out_buf)[i] = INT_MIN;
        raise_warning = 1;
      }
      if (((int64_t *)in_buf)[i] < INT_MIN) {
        ((int32_t *)out_buf)[i] = INT_MIN;
        raise_warning = 1;
      }
    }
  } else {
    for (i=0; i<n; i++) {
      ((int *)out_buf)[i] = ((uint64_t *)in_buf)[i];
    }
    for (i=0; i<n; i++) {
      if (((uint64_t *)in_buf)[i] > INT_MAX) {
        ((int *)out_buf)[i] = INT_MIN;
        raise_warning = 1;
      }
    }
  }
  
  return(raise_warning);
  
}
