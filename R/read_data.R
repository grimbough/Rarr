read_zarr <- function(zarr_file, index) {
  
  metadata <- read_metadata(zarr_file)
  
  required_chunks <- as.matrix(find_chunks_needed(metadata, index))
  
  output <- array(dim = vapply(index, length, integer(1)))
  
  for(i in seq_len(nrow(required_chunks))) {
    
    ## find which elements in the output we will replace
    index_in_result <- list()
    for(j in seq_len(ncol(required_chunks))) {
      index_in_result[[j]] <- which((index[[j]]-1) %/% metadata$chunks[[j]] == required_chunks[i,j])
    }
    
    ## read this chunk
    chunk <- read_chunk(zarr_file, 
                        chunk_id = paste(required_chunks[i,], collapse = "."),
                        metadata = metadata)
    
    index_in_chunk <- list()
    for(j in seq_len(ncol(required_chunks))) {
      which_indices <- which((index[[j]]-1) %/% metadata$chunks[[j]] == required_chunks[i,j])
      index_in_chunk[[j]] <- ((index[[j]][which_indices]-1) %% metadata$chunks[[j]])+1
    }
    
    
    ## extract the required elements and insert into our output array
    selection <- do.call("[", args = c(list(chunk), index_in_chunk, drop = FALSE))
    output <- do.call("[<-", args = c(list(output), index_in_result, list(selection)))
  }
  
  return(output)
  
}


find_chunks_needed <- function(metadata, index) {
  
  index_chunks <- list()
  for(i in seq_along(index)) {
    index_chunks[[i]] <- unique((index[[i]]-1) %/% metadata$chunks[[i]])
  }
  
  required_chunks <- expand.grid(index_chunks)
  return(required_chunks)
  
}


read_chunk <- function(zarr_file, chunk_id, metadata) {
  
  chunk_file <- file.path(zarr_file, chunk_id)
  size <- file.size(chunk_file)
  chunk_dim <- unlist(metadata$chunks)
  
  
  compressed_chunk <- readBin(con = chunk_file, what = "raw", n = size)
  
  uncompressed_chunk <- .Call("read_chunk", compressed_chunk, chunk_dim, PACKAGE = "Rarr")
  
  return(uncompressed_chunk)
  
}

