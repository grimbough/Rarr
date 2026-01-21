.truncate_overflow <- function(x, nbytes) {
  max_value <- 2L^(nbytes * 8L - 1L) - 1L
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
