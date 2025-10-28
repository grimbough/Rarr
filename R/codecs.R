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

# -- Array to bytes -------------------
codec_endian_encode <- function(raw_obj, endian, bytesize) {
  if (is.na(endian) || identical(endian, .Platform$endian)) {
    return(raw_obj)
  }

  ind <- rep_len(rev(seq_len(bytesize)), length(raw_obj)) + (seq_along(raw_obj) - 1) %/% bytesize * bytesize
  return(raw_obj[ind])
}

codec_endian_decode <- codec_endian_encode
