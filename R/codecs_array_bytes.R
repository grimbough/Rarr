# -- Endian ---------------------------------------
codec_bytes_decode <- function(
  decompressed_chunk,
  chunk_dim,
  datatype,
  endian
) {
  if (datatype$base_type == "unicode") {
    ints <- readBin(
      decompressed_chunk,
      what = "integer",
      size = 4,
      n = length(decompressed_chunk) / 4,
      endian = endian
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
  } else if (datatype$base_type == "structured") {
    field <- rep_len(
      rep(
        seq_along(datatype$nbytes),
        datatype$nbytes
      ),
      length.out = length(decompressed_chunk)
    )

    converted_chunk <- vector("list", prod(chunk_dim))
    for (i in seq_along(datatype$nbytes)) {
      type <- datatype[[i]]
      raw_field <- decompressed_chunk[field == i]
      field_converted <- codec_bytes_decode(
        raw_field,
        chunk_dim = NULL,
        type,
        endian = type$endian
      )
      converted_chunk <- Map(
        f = c,
        converted_chunk,
        field_converted
      )
    }
  } else {
    bytesize <- ifelse(
      # For unicode, nbytes actually is sizeof(int) * nchar
      datatype$base_type == "unicode",
      4L,
      datatype$nbytes
    )
    if (!is.na(endian) && endian != .Platform$endian) {
      ind <- rep_len(rev(seq_len(bytesize)), length(decompressed_chunk)) +
        (seq_along(decompressed_chunk) - 1) %/% bytesize * bytesize
      decompressed_chunk <- decompressed_chunk[ind]
    }

    converted_chunk <- .Call(
      paste0("type_convert_", datatype$base_type),
      decompressed_chunk,
      datatype$nbytes,
      PACKAGE = "Rarr"
    )
  }
  dim(converted_chunk) <- chunk_dim
  return(converted_chunk)
}

codec_bytes_encode <- function(d, datatype) {
  if (is.character(d)) {
    ## we need to create fixed length strings either via padding or trimming
    if (datatype$base_type == "unicode") {
      to <- ifelse(datatype$endian == "little", "UCS-4LE", "UCS-4BE")
      raw_list <- iconv(d, to = to, toRaw = TRUE)
    } else {
      raw_list <- iconv(d, toRaw = TRUE)
    }
    unlist(
      lapply(
        raw_list,
        FUN = function(x, nbytes) {
          if (!is.null(x)) {
            length(x) <- nbytes
          }
          return(x)
        },
        nbytes = datatype$nbytes
      )
    )
  } else {
    if (is.na(datatype$endian)) {
      datatype$endian <- "little"
    }
    writeBin(d, raw(), size = datatype$nbytes, endian = datatype$endian)
  }
}


# -- Variable-length UTF-8 ------------------------
codec_vlen_utf8_encode <- function(input, ...) {
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

codec_vlen_utf8_decode <- function(input, chunkdim, ...) {
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
