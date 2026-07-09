is.compact <- function(x) {
  .Call("is_compact", x, PACKAGE = "Rarr")
}

reindex <- function(x, from = 1L, to = 0L) {
  offset <- from - to
  if (is.compact(x)) {
    res <- (min(x) - offset):(max(x) - offset)
  } else {
    res <- x - offset
  }
  return(res)
}
