is.compact <- function(x) {
  .Call("is_compact", x, PACKAGE = "Rarr")
}

reindex <- function(x, from = 1L, to = 0L) {
  offset <- from - to
  if (is.compact(x)) {
    if (is.unsorted(x)) {
      res <- (min(x) - offset):(max(x) - offset)
    } else {
      res <- (x[1L] - offset):(x[length(x)] - offset)
    }
  } else {
    res <- x - offset
  }
  return(res)
}
