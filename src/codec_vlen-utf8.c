#include "codec_vlen-utf8.h"
#include <string.h>

/* Decode a vlen-utf8 encoded raw vector (numcodecs VLenUTF8 format).
 *
 * Format (all integers little-endian):
 *   [4 bytes] nvalues
 *   repeated nvalues times:
 *     [4 bytes] nbytes_i
 *     [nbytes_i bytes] utf-8 string data (no NUL terminator)
 */
SEXP codec_vlen_utf8_decode_c(SEXP input, SEXP chunk_dim) {

  const unsigned char *buf = RAW(input);
  const R_xlen_t buf_len = xlength(input);

  if (buf_len < 4)
    error("vlen-utf8 buffer too short to contain nvalues header");

  /* Read nvalues (little-endian uint32) */
  uint32_t nvalues;
  memcpy(&nvalues, buf, 4);
  size_t pos = 4;

  SEXP data = PROTECT(allocVector(STRSXP, (R_xlen_t)nvalues));

  for (uint32_t i = 0; i < nvalues; i++) {
    if (pos + 4 > (size_t)buf_len)
      error("vlen-utf8 buffer too short reading length of element %u", i);

    uint32_t nbytes;
    memcpy(&nbytes, buf + pos, 4);
    pos += 4;

    if (pos + nbytes > (size_t)buf_len)
      error("vlen-utf8 buffer too short reading data of element %u", i);

    SET_STRING_ELT(data, i, mkCharLenCE((const char *)(buf + pos), (int)nbytes, CE_UTF8));
    pos += nbytes;
  }

  /* Set dim attribute if chunk_dim is not NULL / NA */
  if (!isNull(chunk_dim) && xlength(chunk_dim) > 0) {
    Rf_dimgets(data, chunk_dim);
  }

  UNPROTECT(1);
  return data;
}
