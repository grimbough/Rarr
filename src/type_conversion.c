#include "type_conversion.h"

SEXP type_convert_int(SEXP input, SEXP _n_bytes) {

  int n_bytes = INTEGER(_n_bytes)[0];
  R_xlen_t length = xlength(input);
  void* raw_buffer = RAW(input);

  int *p_data;
  SEXP data;
  R_xlen_t data_length = length / n_bytes;
  R_xlen_t i;

  // space for the converted output
  data = PROTECT(allocVector(INTSXP, data_length));
  p_data = INTEGER(data);

  if(n_bytes == 1) {
    for (i = 0; i < data_length; i++) {
      p_data[i] = ((int8_t *)raw_buffer)[i];
    }
  } else if(n_bytes == 2) {
    int16_t *mock_buffer = (int16_t *)raw_buffer;
    for (i = 0; i < data_length; i++) {
      p_data[i] = mock_buffer[0];
      mock_buffer++;
    }
  } else if(n_bytes == 4) {
    memcpy(p_data, raw_buffer, length);
  } else if (n_bytes == 8) {
    // for now we convert to 32bit int and overflow values are NA_integer
    int bit64conversion = 0;
    if (bit64conversion == 0) {
      int64_to_int32(raw_buffer, data_length, p_data, true);
    }
  }

  UNPROTECT(1);
  return(data);
}

SEXP type_convert_uint(SEXP input, SEXP _n_bytes) {

  int n_bytes = INTEGER(_n_bytes)[0];
  R_xlen_t length = xlength(input);
  void* raw_buffer = RAW(input);

  int *p_data;
  SEXP data;
  R_xlen_t data_length = length / n_bytes;
  R_xlen_t i;

  // space for the converted output
  data = PROTECT(allocVector(INTSXP, data_length));
  p_data = INTEGER(data);

  if(n_bytes == 1) {
    for (i = 0; i < data_length; i++) {
      p_data[i] = ((uint8_t *)raw_buffer)[i];
    }
  } else if(n_bytes == 2) {
    uint16_t *mock_buffer = (uint16_t *)raw_buffer;
    for (i = 0; i < data_length; i++) {
      p_data[i] = mock_buffer[0];
      mock_buffer++;
    }
  } else if(n_bytes == 4) {
    uint32_to_int32(raw_buffer, data_length, p_data);
  } else if (n_bytes == 8) {
    // for now we convert to 32bit int and overflow values are NA_integer
    int bit64conversion = 0;
    if (bit64conversion == 0) {
      int64_to_int32(raw_buffer, data_length, p_data, false);
    }
  }

  UNPROTECT(1);
  return(data);
}

SEXP type_convert_float(SEXP input, SEXP _n_bytes){

  int n_bytes = INTEGER(_n_bytes)[0];
  R_xlen_t length = xlength(input);
  void* raw_buffer = RAW(input);

  R_xlen_t data_length, i;
  double *p_data;
  SEXP data;

  data_length  = length / n_bytes;
  data = PROTECT(allocVector(REALSXP, data_length));
  p_data = REAL(data);

  if(n_bytes == 2) {

    uint16_t *mock_buffer = (uint16_t *)raw_buffer;
    for (i = 0; i < data_length; i++) {
      p_data[i] = (double)float16_to_float64(mock_buffer[0]);
      mock_buffer++;
    }

  } else if(n_bytes == 4) {

    float *mock_buffer = (float *)raw_buffer;
    for (i = 0; i < data_length; i++) {
      p_data[i] = (double)mock_buffer[0];
      mock_buffer++;
    }

  } else if (n_bytes == 8) {
    memcpy(p_data, raw_buffer, length);
  } else {
    error("%d byte floating point values are not currently supported\n", n_bytes);
  }

  UNPROTECT(1);
  return(data);
}

SEXP type_convert_bool(SEXP input, SEXP _n_bytes) {

  R_xlen_t length = xlength(input);
  void* raw_buffer = RAW(input);

  int *p_data;
  SEXP data;

  R_xlen_t data_length = length;

  data = PROTECT(allocVector(LGLSXP, data_length));
  p_data = LOGICAL(data);

  for (int i = 0; i < data_length; i++) {
    p_data[i] = ((int8_t *)raw_buffer)[i];
  }

  UNPROTECT(1);
  return(data);
}

SEXP type_convert_string(SEXP input, SEXP _n_bytes) {

  int n_bytes = INTEGER(_n_bytes)[0];
  R_xlen_t length = xlength(input);
  void* raw_buffer = RAW(input);

  R_xlen_t data_length = length / n_bytes;
  R_xlen_t i;
  SEXP data;

  data = PROTECT(allocVector(STRSXP, data_length));

  for (i = 0; i < data_length; i++) {
    size_t len =  strlen((char *)raw_buffer + i * n_bytes);
    // Read up to max length or NUL terminator.
    // We cannot do one without the other as strings may not be NUL terminated (truncated) and
    // mkCharLenCE complains about NUL characters in the string.
    if (len > n_bytes)
      SET_STRING_ELT(data, i, mkCharLenCE((char *)raw_buffer + i * n_bytes, n_bytes, CE_BYTES));
    else 
      SET_STRING_ELT(data, i, mkCharCE((char *)raw_buffer + i * n_bytes, CE_BYTES));
  }

  UNPROTECT(1);
  return(data);
}