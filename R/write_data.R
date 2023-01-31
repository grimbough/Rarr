

.write_new_zarr_array <- function(x, path, chunk_dim, compressor = use_zlib()) {
  
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
  
  ## iterate over each chunk
  for(i in seq_len(nrow(chunk_names))) {
    
    chunk_path <- paste0(path, paste(chunk_names[i,], collapse = "."))
    
    idx_in_array <- list(); 
    for(j in seq_along(dim(x))) { 
      idx_in_array[[j]] <- which((seq_len(dim(x)[j])-1) %/% chunk_dim[j] == chunk_names[i,j]) 
    }
    
    chunk_in_mem <- R.utils::extract(x, indices = idx_in_array)
    
    compressed_chunk <- .compress_chunk(input_chunk = chunk_in_mem, compressor = compressor)
    
    writeBin(compressed_chunk, con = chunk_path)
    
  }
  
  .write_zarray(path = paste0(path, ".zarray"), array_shape = dim(x), chunk_shape = chunk_dim, 
                data_type = data_type,
                compressor = compressor)
  
  invisible(return(TRUE))
}

.update_zarr_array <- function(path, x,  index) {
  
  stopifnot(is.list(index))
  
  metadata <- read_array_metadata(path)
  zarr_dim <- unlist(metadata$shape)
  chunk_dim <- unlist(metadata$chunks)
  
  chunk_names <- expand.grid(lapply(zarr_dim %/% chunk_dim, seq_len)) - 1
  
  ## coerce x to the same shape as the zarr to be updated
  x <- array(x, dim = vapply(index, length, integer(1)))
  
  for(i in seq_len(nrow(chunk_names))) {
    
    chunk_path <- paste0(path, paste(chunk_names[i,], collapse = "."))
    
    idx_in_zarr <- idx_in_x <- list(); 
    for(j in seq_along(zarr_dim)) { 
      idx_in_zarr[[j]] <- index[[j]][ which((index[[j]]-1) %/% chunk_dim[j] == chunk_names[i,j]) ]
      idx_in_x[[j]] <- which((index[[j]]-1) %/% chunk_dim[j] == chunk_names[i,j])
    }
    
    ## only read and update this chunk if there are some values to be changed
    if(all(vapply(idx_in_zarr, function(x) { length(x) > 0 }, logical(1)))){
      
      idx_in_chunk <- list()
      for(j in seq_along(zarr_dim)) { 
        idx_in_chunk[[j]] <- ((idx_in_zarr[[j]]-1) %% chunk_dim[[j]])+1
      }
      
      chunk_in_mem <- read_chunk(zarr_file = path, chunk_id = chunk_names[i,], metadata = metadata)[["chunk_data"]]
      chunk_in_mem <- .extract_and_replace(chunk_in_mem, idx_in_chunk, R.utils::extract(x, indices = idx_in_x))
      
      ## re-compress updated chunk and write back to disk
      compressed_chunk <- .compress_chunk(input_chunk = chunk_in_mem, compressor = metadata$compressor)
      writeBin(compressed_chunk, con = chunk_path)
    }
  }
  invisible(return(TRUE))
}

.compress_chunk <- function(input_chunk, compressor = use_blosc()) {
  
  ## the compression tools need a raw vector
  ## any permuting for "C" ordering needs to happen before this
  raw_chunk <- .as_raw(as.vector(input_chunk))
  
  if(compressor$id == "blosc") {
    compressed_chunk <- .Call("compress_chunk_BLOSC", raw_chunk, PACKAGE = "Rarr")
  } else if (compressor$id %in% c("zlib", "gzip")) {
    compressed_chunk <- memCompress(from = raw_chunk, type = "gzip")
  } else if (compressor$id == "bz2") {
    compressed_chunk <- memCompress(from = raw_chunk, type = "bzip2")
  } else if (compressor$id == "lzma") {
    compressed_chunk <- memCompress(from = raw_chunk, type = "xz")
  #} else if (compressor$id == "lz4") {
    ## numpy codecs stores the original size of the buffer in the first 4 bytes; we exclude those
  #  compressed_chunk <- .Call("decompress_chunk_LZ4", compressed_chunk[-(1:4)], as.integer(buffer_size), PACKAGE = "Rarr")
  } else {
    stop("Unsupported compression tool")
  }
  
  return(compressed_chunk)
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