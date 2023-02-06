#' Read a Zarr array
#'
#' @param zarr_array_path Path to a Zarr array. A character vector of length 1. This
#'   can either be a location on a local file system or the URI to an array in
#'   S3 storage.
#' @param index A list of the same length as the number of dimensions in the
#'   Zarr array.  Each entry in the list provides the indices in that dimension
#'   that should be read from the array.  Setting a list entry to `NULL` will
#'   read everything in the associated dimension.  If this argument is missing
#'   the entirety of the the Zarr array will be read.
#'
#' @returns An array with the same number of dimensions as the input array. The
#'   extent of each dimension will correspond to the length of the values
#'   provided to the `index` argument.
#'
#' @examples
#'
#' ## Using a local file provided with the package
#' ## This array has 3 dimensions
#' z1 <- system.file("extdata", "zarr_examples", "row-first",
#'                   "int32.zarr", package = "Rarr")
#'
#' ## read the entire array
#' read_zarr_array(zarr_array_path = z1)
#'
#' ## extract values for first 10 entries in the x-dimension, all entries in the y-dimension
#' ## first entry in the z-dimension
#' read_zarr_array(zarr_array_path = z1, index = list(1:10, NULL, 1))
#'
#' \donttest{
#' ## using a Zarr file hosted on Amazon S3
#' ## This array has a single dimension with length 128
#' z2 <- "https://s3.us-west-2.amazonaws.com/cmip6-pds/CMIP3/BCCR/bccr_bcm2_0/piControl/r1i1p1f1/Amon/psl/lon"
#'
#' ## read the entire array
#' read_zarr_array(zarr_array_path = z2)
#'
#' ## read alternating elements
#' read_zarr_array(zarr_array_path = z2, index = list(seq(1, 128, 2)))
#' }
#'
#' @export
read_zarr_array <- function(zarr_array_path, index) {
  
  zarr_array_path <- .normalize_array_path(zarr_array_path)
  ## determine if this is a local or S3 array
  s3_provider <- s3_provider(path = zarr_array_path)
  
  metadata <- read_array_metadata(zarr_array_path, s3_provider = s3_provider)
  
  ## if no index provided we will return everything
  if(missing(index)) { index <- vector(mode = "list", length = length(metadata$shape)) }
  index <- check_index(index = index, metadata = metadata)
  
  required_chunks <- as.matrix(find_chunks_needed(metadata, index))
  
  res <- read_data(required_chunks, zarr_array_path, s3_provider, index, metadata)
  
  if(isTRUE(res$warn > 0)) {
    warning("Integer overflow detected in at least one chunk.\n",
            "Overflowing values have been replaced with NA",
            call. = FALSE)
  }
  
  return(res$output)
  
}

#' @importFrom R.utils extract
read_data <- function(required_chunks, zarr_array_path, s3_provider, index, metadata) {
  
  ## predefine our array to be populated from the read chunks
  output <- array(dim = vapply(index, length, integer(1)))
  warn <- 0L
  
  ## hopefully we can eventually do this in parallel
  chunk_selections <- lapply(seq_len(nrow(required_chunks)), FUN = function(i) { 
    
    ## find which elements to select from the chunk and which in the output we will replace
    index_in_result <- index_in_chunk <- list()
    for(j in seq_len(ncol(required_chunks))) {
      index_in_result[[j]] <- which((index[[j]]-1) %/% metadata$chunks[[j]] == required_chunks[i,j])
      which_indices <- which((index[[j]]-1) %/% metadata$chunks[[j]] == required_chunks[i,j])
      index_in_chunk[[j]] <- ((index[[j]][which_indices]-1) %% metadata$chunks[[j]])+1
    }
    
    ## read this chunk
    chunk <- read_chunk(zarr_array_path, 
                        chunk_id = required_chunks[i,],
                        metadata = metadata, 
                        s3_provider = s3_provider)
    
    warn <- chunk$warning[1]
    chunk_data <- chunk$chunk_data
    
    ## extract the required elements from the chunk
    selection <- R.utils::extract(chunk_data, indices = index_in_chunk, drop = FALSE)
    
    return(list(selection, index_in_result, warning = warn))
  })
  
  ## proceed in serial and update the output with each chunk selection in turn
  for(i in seq_along(chunk_selections)) {
    
    index_in_result <- chunk_selections[[i]][[2]]
    output <- .extract_and_replace(output, index_in_result, y = chunk_selections[[i]][[1]])
    warn <- max(warn, chunk_selections[[i]]$warning[1])
    
  }
  return(list(output = output, warn = warn))
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

#' Read a single Zarr chunk
#'
#' @param zarr_array_path A character vector of length 1, giving the path to the
#'   Zarr array
#' @param chunk_id A numeric vector or single data.frame row with length equal
#'   to the number of dimensions of a chunk.
#' @param metadata List produced by `read_array_metadata()` holding the contents of the
#'   `.zarray` file. If missing this function will be called automatically, but
#'   it is probably preferable to pass the meta data rather than read it
#'   repeatedly for every chunk.
#' @param s3_provider Character indicating whether the Zarr is store on S3 and
#'   if so which platform.  Valid entries are "aws" or "other".  Leave as `NULL`
#'   for a file on local storage.
#'
#' @importFrom aws.s3 get_object
#' @keywords Internal
read_chunk <- function(zarr_array_path, chunk_id, metadata, s3_provider = NULL) {
  
  if(missing(metadata)) {
    metadata <- read_array_metadata(zarr_array_path, s3_provider = s3_provider)
  }
  
  dim_separator <- ifelse(is.null(metadata$dimension_separator), 
                          yes = ".", no = metadata$dimension_separator)
  chunk_id <- paste(chunk_id, collapse = dim_separator)
  
  datatype <- .parse_datatype(metadata$dtype)
  chunk_dim <- unlist(metadata$chunks)
  chunk_file <- paste0(zarr_array_path, chunk_id)
  
  if(nzchar(Sys.getenv("RARR_DEBUG"))) { message(chunk_file) }
  
  if(is.null(s3_provider)) {
    size <- file.size(chunk_file)
    if(file.exists(chunk_file)) {
      compressed_chunk <- readBin(con = chunk_file, what = "raw", n = size)
    } else {
      compressed_chunk <- NULL
    }
  } else {
    
    if(s3_provider == "aws") {
      parsed_url <- url_parse_aws(chunk_file)
    } else {
      parsed_url <- .url_parse_other(chunk_file)
    }
    compressed_chunk <- get_object(
      object = parsed_url$object, 
      bucket = parsed_url$bucket, 
      region = parsed_url$region,
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
  
  datatype <- .parse_datatype(metadata$dtype)
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
    converted_chunk[[1]] <- vapply(tmp, rawToChar, character(1), USE.NAMES = FALSE)
    dim(converted_chunk[[1]]) <- chunk_dim
    ## no warning so set the second element to zero
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
  datatype <- .parse_datatype(metadata$dtype)
  buffer_size <- get_chunk_size(datatype, dimensions = metadata$chunks)
  
  if(decompressor == "blosc") {
    uncompressed_chunk <- .Call("decompress_chunk_BLOSC", compressed_chunk, PACKAGE = "Rarr")
  } else if (decompressor == "zlib") {
    uncompressed_chunk <- memDecompress(from = compressed_chunk, type = "gzip", asChar = FALSE)
  } else if (decompressor == "bz2") {
    uncompressed_chunk <- memDecompress(from = compressed_chunk, type = "bzip2", asChar = FALSE)
  } else if (decompressor == "lzma") {
    uncompressed_chunk <- memDecompress(from = compressed_chunk, type = "xz", asChar = FALSE)
  } else if (decompressor == "lz4") {
    ## numpy codecs stores the original size of the buffer in the first 4 bytes; we exclude those
    uncompressed_chunk <- .Call("decompress_chunk_LZ4", compressed_chunk[-(1:4)], as.integer(buffer_size), PACKAGE = "Rarr")
  } else {
    stop("Unsupported compression tool")
  }
  
  return(uncompressed_chunk)
}


