use_blosc <- function() {
  res <- list(id = "blosc", cname = "lz4", clevel = 5, shuffle = as.integer(TRUE))
  return(res)
}

use_zlib <- function() {
  res <- list(id = "zlib", level = 6)
  return(res)
}

use_lzma <- function() {
  res <- list(id = "lzma", format = 1, level = "9")
  return(res)
}
