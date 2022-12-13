#' @export
read_zarr_array <- function(zarr_array, index) {
  
  is_s3 <- ifelse(grepl(pattern = "^s3://", x = zarr_array), yes = TRUE, no = FALSE)
  
  metadata <- read_array_metadata(zarr_array, is_s3 = is_s3)
  
  required_chunks <- as.matrix(find_chunks_needed(metadata, index))
  
  output <- array(dim = vapply(index, length, integer(1)))
  
  for(i in seq_len(nrow(required_chunks))) {
    
    ## find which elements in the output we will replace
    index_in_result <- list()
    for(j in seq_len(ncol(required_chunks))) {
      index_in_result[[j]] <- which((index[[j]]-1) %/% metadata$chunks[[j]] == required_chunks[i,j])
    }
    
    ## read this chunk
    chunk <- read_chunk(zarr_array, 
                        chunk_id = paste(required_chunks[i,], collapse = "."),
                        metadata = metadata, is_s3 = is_s3)
    
    if(metadata$order == "C") {
      chunk <- aperm(chunk)
    }
    
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

get_chunk_size <- function(datatype, dimensions) {
  
  ## determine the size of the R datatype we're going to return
  sizeof <- switch(datatype$base_type,
                   "logical"  = 4,
                   "integer"  = 4,
                   "uinteger" = 4,
                   "numeric"  = 8,
                   #       "complex",
                   #  "timedelta",
                   #    "datetime",
                   #   "character",
                   #   "unicode",
                   "other" = 1)
  
  buffer_size <- prod(unlist(dimensions), sizeof)
  
  return(buffer_size)
}

#' @importFrom aws.s3 get_object
read_chunk <- function(zarr_file, chunk_id, metadata, is_s3 = FALSE) {
  
  if(missing(metadata)) {
    metadata <- read_array_metadata(zarr_file, is_s3 = is_s3)
  }
  
  datatype <- parse_datatype(metadata$dtype)
  chunk_dim <- unlist(metadata$chunks)
  chunk_file <- file.path(zarr_file, chunk_id)
  
  if(!is_s3) {
    size <- file.size(chunk_file)
    compressed_chunk <- readBin(con = chunk_file, what = "raw", n = size)
  } else {
    
    parsed_url <- url_parse(zarr_file)
    bucket <- str_extract(parsed_url$path, pattern = "^/([[:alnum:]-]*)") %>% 
      str_remove("/")
    object <- str_remove(string = parsed_url$path, pattern = "^/[[:alnum:]-_]*/") |>
      paste(chunk_id, sep = "/")
    
    compressed_chunk <- get_object(object = object, 
                                   bucket = bucket, 
                                   region = "",
                                   base_url = parsed_url$hostname)
  }
  
  
  uncompressed_chunk <- decompress_chunk(compressed_chunk, metadata)  
  
  
  output_type <- switch(datatype$base_type,
                        "boolean" = 0L,
                        "int" = 1L,
                        "uint" = 1L,
                        "float" = 2L)
  
  converted_chunk <- .Call("type_convert_chunk", uncompressed_chunk, 
                           output_type, datatype$nbytes, datatype$is_signed,
                           chunk_dim, PACKAGE = "Rarr")
  
  return(converted_chunk)
  
}

decompress_chunk <- function(compressed_chunk, metadata) {
  
  decompressor <- metadata$compressor$id
  datatype <- parse_datatype(metadata$dtype)
  buffer_size <- get_chunk_size(datatype, dimensions = metadata$chunks)
  
  if(decompressor == "zlib") {
    uncompressed_chunk <- .Call("decompress_chunk_ZLIB", compressed_chunk, as.integer(buffer_size), PACKAGE = "Rarr")
  } else if (decompressor == "blosc") {
    uncompressed_chunk <- .Call("decompress_chunk_BLOSC", compressed_chunk, PACKAGE = "Rarr")
  } else {
    stop("Unknown compression tool")
  }
  
  return(uncompressed_chunk)
}

check_index <- function(index, metadata) {
  
}

