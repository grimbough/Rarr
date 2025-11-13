.format_string <- function(decompressed_chunk, datatype) {
  ## break raw vector into list where each element is the bytes for 1 string
  tmp <- split(
    x = decompressed_chunk,
    f = ceiling(seq_along(decompressed_chunk) / datatype$nbytes)
  )
  converted_chunk <- list(
    vapply(tmp, rawToChar, character(1), USE.NAMES = FALSE),
    0L ## no warning so set the second element to zero
  )
  return(converted_chunk)
}

.format_unicode <- function(decompressed_chunk, datatype) {
  tmp <- split(
    x = decompressed_chunk,
    f = ceiling(seq_along(decompressed_chunk) / datatype$nbytes)
  )
  converted_chunk <- list(
    vapply(
      tmp,
      FUN = function(x) {
        intToUtf8(readBin(x, what = "integer", size = 4, n = length(x) / 4))
      },
      FUN.VALUE = character(1),
      USE.NAMES = FALSE
    ),
    0L
  )
  return(converted_chunk)
}

## for now we're going to assume you only get here with a VLEN UTF8 datatype
.format_object <- function(decompressed_chunk, metadata, datatype) {
  converted_chunk <- list(
    .readVlenUTF8(decompressed_chunk),
    0L
  )
  return(converted_chunk)
}

.readVlenUTF8 <- function(input) {
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
