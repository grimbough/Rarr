# -- Endian ---------------------------------------
codec_bytes_decode <- function(decompressed_chunk, chunk_dim, metadata) {
  datatype <- metadata$datatype
  bytesize <- ifelse(
    # For unicode, nbytes actually is sizeof(int) * nchar
    datatype$base_type == "unicode",
    4L,
    datatype$nbytes
  )

  endian <- metadata$codecs[["bytes"]]$configuration %||% NA_character_
  if (!is.na(endian) && endian != .Platform$endian) {
    ind <- rep_len(rev(seq_len(bytesize)), length(decompressed_chunk)) +
      (seq_along(decompressed_chunk) - 1) %/% bytesize * bytesize
    decompressed_chunk <- decompressed_chunk[ind]
  }

  if (datatype$base_type == "unicode") {
    ints <- readBin(
      decompressed_chunk,
      what = "integer",
      size = 4,
      n = length(decompressed_chunk) / 4
    )
    tmp <- split(
      ints,
      f = ceiling(seq_along(ints) / (datatype$nbytes / 4))
    )
    converted_chunk <- vapply(
      tmp,
      intToUtf8,
      FUN.VALUE = character(1),
      USE.NAMES = FALSE
    )
  } else {
    output_type <- switch(
      datatype$base_type,
      "bool" = 0L,
      "int" = 1L,
      "uint" = 1L,
      "float" = 2L,
      "string" = 3L
    )
    converted_chunk <- .Call(
      "type_convert_chunk",
      decompressed_chunk,
      output_type,
      datatype$nbytes,
      datatype$is_signed,
      PACKAGE = "Rarr"
    )
  }
  dim(converted_chunk) <- chunk_dim
  return(converted_chunk)
}

codec_bytes_encode <- function(raw_obj, endian, bytesize) {
  if (is.na(endian) || endian == .Platform$endian) {
    return(raw_obj)
  }

  ind <- rep_len(rev(seq_len(bytesize)), length(raw_obj)) +
    (seq_along(raw_obj) - 1) %/% bytesize * bytesize
  return(raw_obj[ind])
}


# -- Variable-length UTF-8 ------------------------
codec_vlen_utf8_encode <- function(input, chunkdim, ...) {
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

codec_vlen_utf8_decode <- function(input, ...) {
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
  dim(output) <- chunkdim

  return(output)
}
