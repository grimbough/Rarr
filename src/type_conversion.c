#include "type_conversion.h"

SEXP type_convert_chunk(SEXP input, SEXP _new_type, SEXP _n_bytes, SEXP _is_signed, SEXP Rdim) {
  
  void* p_input = RAW(input);
  SEXP output;
  int new_type = INTEGER(_new_type)[0];
  int n_bytes = INTEGER(_n_bytes)[0];
  int is_signed = (int)LOGICAL(_is_signed)[0];
  
  if(new_type == 0) {
    output = type_convert_LOGICAL(p_input, xlength(input));
  } else if(new_type == 1) {
    output = type_convert_INTEGER(p_input, xlength(input), n_bytes, is_signed);
  } else if (new_type == 2) {
    output = type_convert_REAL(p_input, xlength(input), n_bytes);
  } else {
    error("Unknown data type\n");
  }
  
  setAttrib(VECTOR_ELT(output, 0), R_DimSymbol, Rdim);
  
  UNPROTECT(3);
  return output;
} 

SEXP type_convert_INTEGER(void *raw_buffer, R_xlen_t length, int n_bytes, int is_signed) {
  
  int *p_data;
  SEXP output, data, warning;
  R_xlen_t data_length = length / n_bytes;
  R_xlen_t i;
  
  // space for the converted output
  data = PROTECT(allocVector(INTSXP, data_length));
  p_data = INTEGER(data);
  
  // vector to indicate if a warning has been raised
  warning = PROTECT(allocVector(INTSXP, 1));
  INTEGER(warning)[0] = 0;
  int32_t warn = 0;
  
  if(n_bytes == 1) {
    if(is_signed == 1) {
      for (i = 0; i < data_length; i++) {
        p_data[i] = ((int8_t *)raw_buffer)[i];
      }
    } else {
      for (i = 0; i < data_length; i++) {
        p_data[i] = ((uint8_t *)raw_buffer)[i];
      }
    }
  } else if(n_bytes == 2) {
    
    if(is_signed == 1) {
      int16_t *mock_buffer = (int16_t *)raw_buffer;
      for (i = 0; i < data_length; i++) {
        p_data[i] = mock_buffer[0];
        mock_buffer++;
      }
    } else {
      uint16_t *mock_buffer = (uint16_t *)raw_buffer;
      for (i = 0; i < data_length; i++) {
        p_data[i] = mock_buffer[0];
        mock_buffer++;
      }
    }
        
  } else if(n_bytes == 4) {
    
    if(is_signed == 1) {
      memcpy(p_data, raw_buffer, length);
    } else {
      warn = uint32_to_int32(raw_buffer, data_length, p_data);
      INTEGER(warning)[0] = warn;
    }
    
  } else if (n_bytes == 8) {
    // for now we convert to 32bit int and overflow values are NA_integer
    int bit64conversion = 0;
    if (bit64conversion == 0) { 
      warn = int64_to_int32(raw_buffer, data_length, p_data, is_signed);
    }
    INTEGER(warning)[0] = warn;
  }
  
  output = PROTECT(allocVector(VECSXP, 2));
  SET_VECTOR_ELT(output, 0, data);
  SET_VECTOR_ELT(output, 1, warning);
    
  return(output);
}

SEXP type_convert_REAL(void *raw_buffer, R_xlen_t length, int n_bytes) {
  
  R_xlen_t data_length, i;
  double *p_data;
  SEXP output, data, warning;
  
  data_length  = length / n_bytes;
  data = PROTECT(allocVector(REALSXP, data_length));
  p_data = REAL(data);
  warning = PROTECT(allocVector(INTSXP, 1));
  INTEGER(warning)[0] = 0;
  
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
  
  output = PROTECT(allocVector(VECSXP, 2));
  SET_VECTOR_ELT(output, 0, data);
  SET_VECTOR_ELT(output, 1, warning);
  
  return(output);
}

SEXP type_convert_LOGICAL(void *raw_buffer, R_xlen_t length) {
  
  int *p_data;
  SEXP output, data, warning;
  
  R_xlen_t data_length = length;
  
  data = PROTECT(allocVector(LGLSXP, data_length));
  p_data = LOGICAL(data);
  warning = PROTECT(allocVector(INTSXP, 1));
  INTEGER(warning)[0] = 0;
  
  for (int i = 0; i < data_length; i++) {
    p_data[i] = ((int8_t *)raw_buffer)[i];
  }
  
  output = PROTECT(allocVector(VECSXP, 2));
  SET_VECTOR_ELT(output, 0, data);
  SET_VECTOR_ELT(output, 1, warning);
  
  return(output);
}