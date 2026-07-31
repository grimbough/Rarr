.get_chunk_size <- function(chunk_dim, datatype) {
  # Max size of an uncompressed chunk.
  # This is faster than using file.size() because it avoids a system call.
  return(prod(c(chunk_dim, datatype$nbytes, 8L)))
}
