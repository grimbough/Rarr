# -- Array to array -------------------
codec_transpose_encode <- function(array, indices = seq_along(dim(array))) {
  dim(array) <- dim(array)[indices]
  array <- aperm(array, indices)

  return(array)
}

codec_transpose_decode <- function(array, indices = seq_along(dim(array))) {
  inv_indices <- order(indices)
  array <- aperm(array, inv_indices)
  dim(array) <- dim(array)[inv_indices]

  return(array)
}
