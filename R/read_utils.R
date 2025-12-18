.format_string <- function(decompressed_chunk, datatype) {
  ## break raw vector into list where each element is the bytes for 1 string
  nwords <- length(decompressed_chunk) / datatype$nbytes

  converted_chunk <- list(
    readBin(
      decompressed_chunk,
      "character",
      size = datatype$nbytes,
      n = nwords
    ),
    0L ## no warning so set the second element to zero
  )
  return(converted_chunk)
}

.format_unicode <- function(decompressed_chunk, datatype) {
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
  converted_chunk <- list(
    vapply(
      tmp,
      intToUtf8,
      FUN.VALUE = character(1),
      USE.NAMES = FALSE
    ),
    0L
  )
  return(converted_chunk)
}
