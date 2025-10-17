# -- Array to array -------------------
codec_transpose_encode <- function(array, indices = seq_along(dim(array))) {
  dim(array) <- dim(array)[indices]
  array <- aperm(array, indices)

  return(array)
}
