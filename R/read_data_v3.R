
#' @importFrom R.utils extract
read_data_v3 <- function(required_chunks, zarr_array_path, s3_client, 
                      index, metadata) {
  
  warn <- 0L
  
  ## hopefully we can eventually do this in parallel
  chunk_selections <- lapply(seq_len(nrow(required_chunks)), FUN = function(i) {
    ## find elements to select from the chunk and what in the output we replace
    index_in_result <- index_in_chunk <- list()
    alt_chunk_dim <- unlist(metadata$chunks)
    
    for (j in seq_len(ncol(required_chunks))) {
      index_in_result[[j]] <- which((index[[j]] - 1) %/% metadata$chunks[[j]] == required_chunks[i, j])
      ## are we requesting values outside the array due to overhanging chunks?
      outside_extent <- index_in_result[[j]] > metadata$shape[[j]]
      if (any(outside_extent))
        index_in_result[[j]] <- index_in_result[[j]][-outside_extent]
      if (any(index_in_result[[j]] == metadata$shape[[j]])) 
        alt_chunk_dim[j] <- length(index_in_result[[j]])
      
      index_in_chunk[[j]] <- ((index[[j]][index_in_result[[j]]] - 1) %% metadata$chunks[[j]]) + 1
    }
    
    ## read this chunk
    chunk <- read_chunk_v3(zarr_array_path,
                        chunk_id = required_chunks[i, ],
                        metadata = metadata,
                        s3_client = s3_client,
                        alt_chunk_dim = alt_chunk_dim
    )
    warn <- chunk$warning[1]
    chunk_data <- chunk$chunk_data
    
    ## extract the required elements from the chunk
    selection <- R.utils::extract(chunk_data, indices = index_in_chunk, drop = FALSE)
    
    return(list(selection, index_in_result, warning = warn))
  })
  
  ## predefine our array to be populated from the read chunks
  output <- array(metadata$fill_value, dim = vapply(index, length, integer(1)))
  
  ## proceed in serial and update the output with each chunk selection in turn
  for (i in seq_along(chunk_selections)) {
    index_in_result <- chunk_selections[[i]][[2]]
    cmd <- .create_replace_call(x_name = "output", idx_name = "index_in_result",
                                idx_length = length(index_in_result), 
                                y_name = "chunk_selections[[i]][[1]]")
    eval(parse(text = cmd))
    warn <- max(warn, chunk_selections[[i]]$warning[1])
  }
  return(list(output = output, warn = warn))
}

#' Read a single Zarr chunk
#'
#' @param zarr_array_path A character vector of length 1, giving the path to the
#'   Zarr array
#' @param chunk_id A numeric vector or single data.frame row with length equal
#'   to the number of dimensions of a chunk.
#' @param metadata List produced by `read_array_metadata()` holding the contents
#'   of the `.zarray` file. If missing this function will be called
#'   automatically, but it is probably preferable to pass the meta data rather
#'   than read it repeatedly for every chunk.
#' @param s3_client Object created by [paws.storage::s3()]. Only required for a
#'   file on S3. Leave as `NULL` for a file on local storage.
#' @param alt_chunk_dim The dimensions of the array that should be created from
#'   this chunk.  Normally this will be the same as the chunk shape in
#'   `metadata`, but when dealing with edge chunks, which may overlap the true
#'   extent of the array the returned array should be smaller than the chunk
#'   shape.
#'
#' @returns A list of length 2.  The entries should be names "chunk_data" and
#'   "warning". The first is an array containing the decompressed chunk values,
#'   the second is an integer indicating whether there were any overflow
#'   warnings generated will reading the chunk into an R datatype.
#'   
#' @keywords Internal
read_chunk_v3 <- function(zarr_array_path, chunk_id, metadata, s3_client = NULL,
                       alt_chunk_dim = NULL) {
  if (missing(metadata)) {
    metadata <- read_array_metadata(zarr_array_path, s3_client = s3_client)
  }
  
  dim_separator <- ifelse(is.null(metadata$dimension_separator),
                          yes = "/", no = metadata$dimension_separator
  )
  chunk_id <- paste(c("c", chunk_id), collapse = dim_separator)
  
  datatype <- .parse_datatype_v3(metadata$data_type)
  chunk_file <- paste0(zarr_array_path, chunk_id)
  
  if (nzchar(Sys.getenv("RARR_DEBUG"))) { message(chunk_file) }
  
  if (is.null(s3_client)) {
    size <- file.size(chunk_file)
    if (file.exists(chunk_file)) {
      compressed_chunk <- readBin(con = chunk_file, what = "raw", n = size)
    } else {
      compressed_chunk <- NULL
    }
  } else {
    
    parsed_url <- parse_s3_path(chunk_file)
    
    if(.s3_object_exists(s3_client, parsed_url$bucket, parsed_url$object)) {
      compressed_chunk <- s3_client$get_object(Bucket = parsed_url$bucket, 
                                               Key = parsed_url$object)$Body
    } else {
      compressed_chunk <- NULL
    }
  }
  
  ## either decompress and format the chunk data
  ## or create a new chunk based on the fill value
  if (!is.null(compressed_chunk)) {
    converted_chunk <- list(
      chunk_data = converted_chunk <- .apply_codecs(compressed_chunk, metadata, datatype),
      warning    = 0L
    )
  } else {
    converted_chunk <- list(
      "chunk_data" = array(metadata$fill_value, dim = unlist(metadata$chunks)),
      "warning"    = 0L
    )
  }
  
  return(converted_chunk)
}

.apply_codecs <- function(chunk, metadata, data_type) {
  
  transpose_applied <- FALSE
  
  n_codecs <- length(metadata$codecs)
  for(i in rev(seq_len(n_codecs))) {
    codec_name <- metadata$codecs[[i]]$name
    config <- metadata$codecs[[i]]$configuration
    if(codec_name == "endian") {
      chunk <- endian_codec(chunk, endian = config$endian, 
                            data_type = data_type, encoding = FALSE)
    } else if(codec_name == "transpose") {
      chunk <- transpose_codec(chunk, chunk_dim = unlist(metadata$chunks), 
                               order = config$order, encoding = FALSE)
      transpose_applied <- TRUE
    } else if (codec_name == "gzip") {
      chunk <- gzip_codec(chunk, encoding = FALSE)
    } else if (codec_name == "blosc") {
      chunk <- blosc_codec(chunk, cname = config$cname, 
                           clevel = config$clevel, shuffle = config$shuffle,
                           encoding = FALSE)
    } else {
      stop("Unsupported codec: ", codec_name)
    }
      
  }
  
  ## Assume we need C ordering if transpose code not specified
  if(!transpose_applied) {
    chunk <- transpose_codec(chunk, chunk_dim = unlist(metadata$chunks), 
                             order = "C", encoding = FALSE)
  }
  
  return(chunk)
}