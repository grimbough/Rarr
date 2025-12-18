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
