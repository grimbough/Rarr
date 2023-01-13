#' @export
read_zarr_array <- function(zarr_array, index) {
  
  is_s3 <- ifelse(grepl(pattern = "^s3://", x = zarr_array), yes = TRUE, no = FALSE)
  
  metadata <- read_array_metadata(zarr_array, is_s3 = is_s3)
  
  ## if no index provided we will return everything
  if(missing(index)) { index <- vector(mode = "list", length = metadata$shape) }
  check_index(index = index, metadata = metadata)
  
  required_chunks <- as.matrix(find_chunks_needed(metadata, index))
  
  output <- array(dim = vapply(index, length, integer(1)))
  
  warn <- 0L
  
  for(i in seq_len(nrow(required_chunks))) {
    
    ## find which elements in the output we will replace
    index_in_result <- list()
    for(j in seq_len(ncol(required_chunks))) {
      index_in_result[[j]] <- which((index[[j]]-1) %/% metadata$chunks[[j]] == required_chunks[i,j])
    }
    
    ## read this chunk
    chunk <- read_chunk(zarr_array, 
                        chunk_id = required_chunks[i,],
                        metadata = metadata, 
                        is_s3 = is_s3)
    
    warn <- max(warn, chunk$warning[1])
    chunk_data <- chunk$chunk_data
    
    index_in_chunk <- list()
    for(j in seq_len(ncol(required_chunks))) {
      which_indices <- which((index[[j]]-1) %/% metadata$chunks[[j]] == required_chunks[i,j])
      index_in_chunk[[j]] <- ((index[[j]][which_indices]-1) %% metadata$chunks[[j]])+1
    }
    
    ## extract the required elements and insert into our output array
    selection <- do.call("[", args = c(list(chunk_data), index_in_chunk, drop = FALSE))
    output <- do.call("[<-", args = c(list(output), index_in_result, list(selection)))
  }
  
  if(isTRUE(warn > 0)) {
    warning("Integer overflow detected in at least one chunk.\n",
            "Overflowing values have been replaced with NA",
            call. = FALSE)
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
                   "boolean" = 4,
                   "int"     = 4,
                   "uint"    = 4,
                   "float"   = 8,
                   # "complex",
                   # "timedelta",
                   # "datetime",
                   "string"  = datatype$nbytes,
                   # "unicode",
                   "other"   = 1)
  
  buffer_size <- prod(unlist(dimensions), sizeof)
  
  return(buffer_size)
}

#' @importFrom aws.s3 get_object
read_chunk <- function(zarr_file, chunk_id, metadata, is_s3 = FALSE) {
  
  if(missing(metadata)) {
    metadata <- read_array_metadata(zarr_file, is_s3 = is_s3)
  }
  
  dim_separator <- ifelse(is.null(metadata$dimension_separator), 
                          yes = ".", no = metadata$dimension_separator)
  chunk_id <- paste(chunk_id, collapse = dim_separator)
  
  datatype <- parse_datatype(metadata$dtype)
  chunk_dim <- unlist(metadata$chunks)
  chunk_file <- file.path(zarr_file, chunk_id)
  
  if(!is_s3) {
    size <- file.size(chunk_file)
    if(file.exists(chunk_file)) {
      compressed_chunk <- readBin(con = chunk_file, what = "raw", n = size)
    } else {
      compressed_chunk <- NULL
    }
  } else {
    
    parsed_url <- url_parse(zarr_file)
    bucket <- str_extract(parsed_url$path, pattern = "^/([[:alnum:]-]*)") |> 
      str_remove("/")
    object <- str_remove(string = parsed_url$path, pattern = "^/[[:alnum:]-_]*/") |>
      paste(chunk_id, sep = "/")
    
    compressed_chunk <- get_object(object = object, 
                                   bucket = bucket, 
                                   region = "",
                                   base_url = parsed_url$hostname)
  }
  
  ## either decompress and format the chunk data
  ## or create a new chunk based on the fill value
  if(!is.null(compressed_chunk)) {
    uncompressed_chunk <- decompress_chunk(compressed_chunk, metadata)  
    converted_chunk <- format_chunk(uncompressed_chunk, metadata)
  } else {
    converted_chunk <- list(
      "chunk_data" = array(data = metadata$fill_value, dim = chunk_dim),
      "warning"    = 0
    )
  }
  
  return(converted_chunk)
  
}

#' @returns A list of length 2.  The first element is the formatted chunk data.
#'   The second is an integer of length 1, indicating if warnings were encountered
#'   when converting types
#' @keywords Internal
format_chunk <- function(uncompressed_chunk, metadata) {
  
  datatype <- parse_datatype(metadata$dtype)
  chunk_dim <- unlist(metadata$chunks)
  ## reverse dimensions for column first datasets
  if(metadata$order == "C") {
    chunk_dim <- rev(chunk_dim)
  }
  
  if(datatype$base_type == "string") {
    ## break raw vector into list where is each element is the bytes for 1 string
    tmp <- split(uncompressed_chunk, rep(seq_len(length(uncompressed_chunk) / datatype$nbytes), 
                                         each = datatype$nbytes))
    converted_chunk <- list()
    converted_chunk[[1]] <- sapply(tmp, rawToChar)
    dim(converted_chunk[[1]]) <- chunk_dim
    converted_chunk[[2]] <- 0L
    
  } else {
    output_type <- switch(datatype$base_type,
                          "boolean" = 0L,
                          "int" = 1L,
                          "uint" = 1L,
                          "float" = 2L)
    converted_chunk <- .Call("type_convert_chunk", uncompressed_chunk, 
                             output_type, datatype$nbytes, datatype$is_signed,
                             chunk_dim, PACKAGE = "Rarr")
  }
  
  ## more manipulation to get the correct dimensions. 
  ## Surely there's a way to do this in one step rather than two??
  if(metadata$order == "C") {
    converted_chunk[[1]] <- aperm(converted_chunk[[1]])
  } 
  
  names(converted_chunk) <- c("chunk_data", "warning")
  return(converted_chunk)
}

decompress_chunk <- function(compressed_chunk, metadata) {
  
  decompressor <- metadata$compressor$id
  datatype <- parse_datatype(metadata$dtype)
  buffer_size <- get_chunk_size(datatype, dimensions = metadata$chunks)
  
  if(decompressor == "blosc") {
    uncompressed_chunk <- .Call("decompress_chunk_BLOSC", compressed_chunk, PACKAGE = "Rarr")
  } else if (decompressor == "zlib") {
    #uncompressed_chunk <- .Call("decompress_chunk_ZLIB", compressed_chunk, as.integer(buffer_size), PACKAGE = "Rarr")
    uncompressed_chunk <- memDecompress(from = compressed_chunk, type = "gzip", asChar = FALSE)
  } else if (decompressor == "bz2") {
    uncompressed_chunk <- memDecompress(from = compressed_chunk, type = "bzip2", asChar = FALSE)
  } else {
    stop("Unsupported compression tool")
  }
  
  return(uncompressed_chunk)
}


