#include "type_conversion.h"

SEXP type_convert_bfloat(SEXP input, SEXP n_bytes, SEXP dims, SEXP endian) {
  const int nbytes = INTEGER(n_bytes)[0];
  if (nbytes != 2)
    Rf_error("Only 2 byte bfloat16 is supported");

  const R_xlen_t length = xlength(input);

  const bool big_endian = strcmp(CHAR(STRING_ELT(endian, 0)), "big") == 0;
  const uint8_t *raw = (const uint8_t *)RAW(input);
  const R_xlen_t data_length = length / nbytes;

  SEXP data = PROTECT(allocVector(REALSXP, data_length));
  double *p_data = REAL(data);

  for (R_xlen_t i = 0; i < data_length; i++) {
    const uint8_t b0 = raw[2 * i];
    const uint8_t b1 = raw[2 * i + 1];
    const uint16_t bf16 =
        big_endian ? (uint16_t)(b0 << 8 | b1) : (uint16_t)(b1 << 8 | b0);
    /* bfloat16 = top 16 bits of float32 */
    const uint32_t f32_bits = (uint32_t)bf16 << 16;
    float f32_val;
    memcpy(&f32_val, &f32_bits, sizeof f32_val);
    p_data[i] = (double)f32_val;
  }

  if (!isNull(dims) && xlength(dims) > 0)
    Rf_dimgets(data, dims);

  UNPROTECT(1);
  return data;
}
