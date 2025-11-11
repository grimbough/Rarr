# -- Array to array -------------------
codec_transpose_encode <- function(array, indices = seq_along(dim(array))) {
  if (identical(indices, seq_along(dim(array)))) {
    return(array)
  }
  dim(array) <- dim(array)[indices]
  array <- aperm(array, indices)

  return(array)
}

codec_transpose_decode <- function(array, indices = seq_along(dim(array))) {
  if (identical(indices, seq_along(dim(array)))) {
    return(array)
  }
  inv_indices <- order(indices)
  array <- aperm(array, inv_indices)
  dim(array) <- dim(array)[inv_indices]

  return(array)
}

# -- Array to bytes -------------------
codec_endian_encode <- function(raw_obj, endian, bytesize) {
  if (is.na(endian) || endian == .Platform$endian) {
    return(raw_obj)
  }

  ind <- rep_len(rev(seq_len(bytesize)), length(raw_obj)) +
    (seq_along(raw_obj) - 1) %/% bytesize * bytesize
  return(raw_obj[ind])
}

codec_endian_decode <- codec_endian_encode

codec_vlen_utf8_encode <- function(input) {
  raw_nvalues <- writeBin(length(input), raw(), size = 4, endian = "little")
  raw_strings <- lapply(input, function(x) charToRaw(enc2utf8(x)))
  raw_string_lens <- lapply(lengths(raw_strings), function(x) {
    writeBin(x, raw(), size = 4, endian = "little")
  })

  raw_vlen_utf8 <- c(
    raw_nvalues,
    unlist(Map(
      function(len, str) c(len, str),
      raw_string_lens,
      raw_strings
    ))
  )

  return(raw_vlen_utf8)
}

codec_vlen_utf8_decode <- function(input) {
  con <- rawConnection(input)
  on.exit(close(con))
  # Looking at numcodecs source code, this is by definition/convention
  # always little-endian
  nvalues <- readBin(con, what = "integer", n = 1, size = 4, endian = "little")
  output <- character(length = nvalues)
  for (i in seq_len(nvalues)) {
    nbytes <- readBin(con, what = "integer", n = 1, size = 4, endian = "little")
    output[i] <- readChar(con, nchars = nbytes, useBytes = TRUE)
  }

  Encoding(output) <- "UTF-8"
  return(output)
}
