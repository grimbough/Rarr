

.write_zarr_array <- function(x, path, chunk_dim) {
  
  path <- .normalize_array_path(path)
  if(!dir.exists(path)) { dir.create(path) }
  
  data_type <- switch(storage.mode(x),
                      "integer" = "<i4",
                      "double"  = "<f8",
                      "character" = "|S",
                      NULL)
  if(data_type == "S") { data_type <- paste0("S", max(nchar(x))) }
  if(is.null(data_type)) { stop("Currently only able to write integer, double, and character arrays") }

  .check_chunk_shape(x_dim = dim(x), chunk_dim = chunk_dim)
  
  chunk_names <- expand.grid(lapply(dim(x) %/% chunk_dim, seq_len)) - 1
  
  for(i in seq_len(nrow(chunk_names))) {
    
    chunk_path <- paste0(path, paste(chunk_names[i,], collapse = "."))
    
    idx_in_array <- list(); 
    for(j in seq_along(dim(x))) { 
      idx_in_array[[j]] <- which((seq_len(dim(x)[j])-1) %/% chunk_dim[j] == chunk_names[i,j]) 
    }
    
    chunk_in_mem <- R.utils::extract(x, indices = idx_in_array)
    
    compressed_chunk <- memCompress(.as_raw(as.vector(chunk_in_mem)), type = "gzip")
    
    writeBin(compressed_chunk, con = chunk_path)
    
  }
  
  .write_zarray(path = paste0(path, ".zarray"), array_shape = dim(x), chunk_shape = chunk_dim, data_type = data_type)
  
  invisible(return(TRUE))
}


.as_raw <- function(d) { writeBin(d, raw()); }


.find_chunks_needed <- function(metadata, index) {
  
  index_chunks <- list()
  for(i in seq_along(index)) {
    index_chunks[[i]] <- unique((index[[i]]-1) %/% metadata$chunks[[i]])
  }
  
  required_chunks <- expand.grid(index_chunks)
  return(required_chunks)
}

.check_chunk_shape <- function(x_dim, chunk_dim) {
  
  if(length(x_dim) != length(chunk_dim)) {
    stop("The dimensions of the chunk must equal the dimensions of the array.")
  }
  
  for(i in seq_along(x_dim)) {
    if((x_dim[i] < chunk_dim[i]) || (chunk_dim[i] < 1)) {
      stop("Chunk dimensions outside the extent of the array")
    }
  }
  
  invisible(return(TRUE))
  
}