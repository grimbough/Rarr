#include "read_chunk.h"

SEXP decompress_chunk(SEXP input) {
  
  void* p_input = RAW(input);
  void *p_output;
  size_t cbytes, blocksize, outbuf_size;
  
  blosc_cbuffer_sizes(p_input, &outbuf_size, &cbytes, &blocksize);
  
  SEXP output = PROTECT(allocVector(RAWSXP, outbuf_size));
  p_output = RAW(output);
  
  blosc_decompress(p_input, p_output, outbuf_size);
  
  UNPROTECT(1);
  return output;
} 


SEXP type_convert_chunk(SEXP input, SEXP new_type, SEXP _n_bytes, SEXP _is_signed, SEXP Rdim) {
  
  void* p_input = RAW(input);
  SEXP output;
  int n_bytes = INTEGER(_n_bytes)[0];
  int is_signed = (int)LOGICAL(_is_signed)[0];

  output = type_convert_INTEGER(p_input, xlength(input), n_bytes, is_signed);
  
  setAttrib(output, R_DimSymbol, Rdim);
  
  UNPROTECT(1);
  return output;
} 

SEXP type_convert_INTEGER(void *raw_buffer, R_xlen_t length, int n_bytes, int is_signed) {
  
  int *p_output;
  SEXP output;
  R_xlen_t output_length = length / n_bytes;
  
  output = PROTECT(allocVector(INTSXP, output_length));
  p_output = INTEGER(output);
  
  if(n_bytes == 1) {
    if(is_signed == 1) {
      for (int i = 0; i < output_length; i++) {
        p_output[i] = ((int8_t *)raw_buffer)[i];
      }
    } else {
      for (int i = 0; i < output_length; i++) {
        p_output[i] = ((uint8_t *)raw_buffer)[i];
      }
    }
  } else if(n_bytes == 2) {
    
    if(is_signed == 1) {
      int16_t *mock_buffer = (int16_t *)raw_buffer;
      for (int i = 0; i < output_length; i++) {
        p_output[i] = mock_buffer[0];
        mock_buffer++;
      }
    } else {
      uint16_t *mock_buffer = (uint16_t *)raw_buffer;
      for (int i = 0; i < output_length; i++) {
        p_output[i] = mock_buffer[0];
        mock_buffer++;
      }
    }
        
  } else if((n_bytes == 4) & (is_signed == 1)) {
    memcpy(p_output, raw_buffer, length);
  }
  
  return(output);
  
}