# -- Transpose -------------------
codec_transpose_encode <- function(array, indices = seq_along(dim(array))) {
  # FIXME: we do two copies here (aperm and dim assignment) which can be expensive for large arrays
  inv_indices <- order(indices)
  array <- aperm(array, inv_indices)
  dim(array) <- dim(array)[inv_indices]

  return(array)
}

codec_transpose_decode <- function(array, indices = seq_along(dim(array))) {
  final_dim <- dim(array)

  # FIXME: we manually dispatch aperm() to t() but eventually, there should be
  # an aperm.matrix() method.
  # When this happens, we only have to handle the drop() case.
  if (sum(dim(array) > 1L) == 2L) {
    array <- drop(array)
    array <- t(array)
    dim(array) <- final_dim
  } else {
    dim(array) <- dim(array)[indices]
    array <- aperm(array, indices)
  }

  return(array)
}
