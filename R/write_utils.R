.unicode_to_int <- function(input, typestr) {
  data_type <- .parse_datatype(typestr)

  nchar <- as.integer(data_type$nbytes / 4L)

  to <- ifelse(data_type$endian == "little", "UCS-4LE", "UCS-4BE")
  raw_list <- iconv(input, to = to, toRaw = TRUE)

  base64_strings <- vapply(
    raw_list,
    function(x) {
      if (length(x) < nchar * 4) {
        x <- c(x, raw((nchar * 4) - length(x)))
      }
      jsonlite::base64_enc(x)
    },
    FUN.VALUE = character(1),
    USE.NAMES = FALSE
  )

  return(base64_strings)
}

.truncate_overflow <- function(x, datatype) {
  max_value <- 2L^(.parse_datatype(datatype)$nbytes * 8L - 1L) - 1L
  min_value <- -max_value - 1L

  positive_overflow <- x > max_value
  negative_overflow <- x < min_value
  if (any(positive_overflow | negative_overflow)) {
    warning(
      "Some values in 'x' are too large to be represented by the ",
      "specified data type. They will be truncated when written.",
      call. = FALSE
    )
    storage.mode(max_value) <- storage.mode(x)
    storage.mode(min_value) <- storage.mode(x)
    x[positive_overflow] <- max_value
    x[negative_overflow] <- min_value
  }
  return(x)
}
