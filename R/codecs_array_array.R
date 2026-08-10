# -- Transpose -------------------
codec_transpose_encode <- function(array, indices = seq_along(dim(array))) {
  # FIXME: we do two copies here (aperm and dim assignment) which can be expensive for large arrays
  inv_indices <- order(indices)
  array <- aperm(array, inv_indices)
  dim(array) <- dim(array)[inv_indices]

  return(array)
}

codec_transpose_decode <- function(array, indices = seq_along(dim(array))) {
  # FIXME: we manually dispatch aperm() to t() but eventually, there should be
  # an aperm.matrix() method.
  # When this happens, we only have to handle the drop() case.

  final_dim <- dim(array)

  to_drop <- final_dim == 1L
  if (any(to_drop)) {
    array <- drop(array)
    indices <- order(indices[!to_drop])
  }

  if (is.matrix(array) && all(indices == c(2L, 1L))) {
    array <- t(array)
    dim(array) <- final_dim
  } else {
    dim(array) <- dim(array)[indices]
    array <- aperm(array, indices)
  }

  return(array)
}
