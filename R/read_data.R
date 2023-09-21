#' Read a Zarr array
#'
#' @param zarr_array_path Path to a Zarr array. A character vector of length 1.
#'   This can either be a location on a local file system or the URI to an array
#'   in S3 storage.
#' @param index A list of the same length as the number of dimensions in the
#'   Zarr array.  Each entry in the list provides the indices in that dimension
#'   that should be read from the array.  Setting a list entry to `NULL` will
#'   read everything in the associated dimension.  If this argument is missing
#'   the entirety of the the Zarr array will be read.
#' @param s3_client Object created by [paws.storage::s3()]. Only required for a
#'   file on S3. Leave as `NULL` for a file on local storage.
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
#'   "int32.zarr",
#'   package = "Rarr"
#' )
#'
#' ## read the entire array
#' read_zarr_array(zarr_array_path = z1)
#'
#' ## extract values for first 10 rows, all columns, first slice
#' read_zarr_array(zarr_array_path = z1, index = list(1:10, NULL, 1))
#'
#' \donttest{
#' ## using a Zarr file hosted on Amazon S3
#' ## This array has a single dimension with length 576
#' z2 <- "https://power-analysis-ready-datastore.s3.amazonaws.com/power_901_constants.zarr/lon/"
#'
#' ## read the entire array
#' read_zarr_array(zarr_array_path = z2)
#'
#' ## read alternating elements
#' read_zarr_array(zarr_array_path = z2, index = list(seq(1, 576, 2)))
#' }
#'
#' @export
read_zarr_array <- function(zarr_array_path, index, s3_client) {
  
  zarr_array_path <- .normalize_array_path(zarr_array_path)
  ## determine if this is a local or S3 array
  if(missing(s3_client)) 
    s3_client <- .create_s3_client(path = zarr_array_path)

  metadata <- read_array_metadata(zarr_array_path, s3_client = s3_client)

  ## if no index provided we will return everything
  if (missing(index)) {
    index <- vector(mode = "list", length = length(metadata$shape))
  }
  index <- check_index(index = index, metadata = metadata)

  required_chunks <- as.matrix(find_chunks_needed(metadata, index))

  res <- read_data(required_chunks, zarr_array_path, s3_client, index, metadata)

  if (isTRUE(res$warn > 0)) {
    warning("Integer overflow detected in at least one chunk.\n",
      "Overflowing values have been replaced with NA",
      call. = FALSE
    )
  }

  return(res$output)
}

#' @importFrom R.utils extract
read_data <- function(required_chunks, zarr_array_path, s3_client, 
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
    chunk <- read_chunk(zarr_array_path,
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

find_chunks_needed <- function(metadata, index) {
  index_chunks <- list()
  for (i in seq_along(index)) {
    index_chunks[[i]] <- unique((index[[i]] - 1) %/% metadata$chunks[[i]])
  }

  required_chunks <- expand.grid(index_chunks)
  return(required_chunks)
}

#' Determine the size of chunk in bytes
#' 
#' @param datatype A list of details for the array datatype.  Expected to be
#' produced by [.parse_datatype()].
#' @param dimensions A list containing the dimensions of the chunk.  Expected 
#' to be found in a list produced by [read_array_metadata()].
#' 
#' @returns An integer giving the size of the chunk in bytes
#'   
#' @keywords Internal
get_chunk_size <- function(datatype, dimensions) {
  ## determine the size of the R datatype we're going to return
  ## TODO: not all datatypes are implemented yet
  sizeof <- switch(datatype$base_type,
    "boolean" = 4L,
    "int"     = 4L,
    "uint"    = 4L,
    "float"   = 8L,
    # "complex",
    # "timedelta",
    # "datetime",
    "string"  = as.integer(datatype$nbytes),
    # "unicode",
    "other"   = 1L
  )

  buffer_size <- prod(unlist(dimensions), sizeof)

  return(as.integer(buffer_size))
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
read_chunk <- function(zarr_array_path, chunk_id, metadata, s3_client = NULL,
                       alt_chunk_dim = NULL) {
  if (missing(metadata)) {
    metadata <- read_array_metadata(zarr_array_path, s3_client = s3_client)
  }

  dim_separator <- ifelse(is.null(metadata$dimension_separator),
    yes = ".", no = metadata$dimension_separator
  )
  chunk_id <- paste(chunk_id, collapse = dim_separator)

  datatype <- .parse_datatype(metadata$dtype)
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
    decompressed_chunk <- .decompress_chunk(compressed_chunk, metadata)
    converted_chunk <- .format_chunk(decompressed_chunk, metadata, alt_chunk_dim)
  } else {
    converted_chunk <- list(
      "chunk_data" = array(metadata$fill_value, dim = unlist(metadata$chunks)),
      "warning"    = 0L
    )
  }

  return(converted_chunk)
}

#' Format the decompressed chunk as an array of the correct type
#'
#' When a chunk is decompressed it is returned as a vector of raw bytes.  This
#' function uses the array metadata to select how to convert the bytes into the
#' final datatype and then converts the resulting output into an array of the
#' appropriate dimensions, including re-ordering if the original data is in
#' row-major order.
#'
#' @param decompressed_chunk Raw vector holding the decompressed bytes for this
#'   chunk.
#' @param metadata List produced by `read_array_metadata()` holding the contents
#'   of the `.zarray` file.
#' @param alt_chunk_dim The dimensions of the array that should be created from
#'   this chunk.  Normally this will be the same as the chunk shape in
#'   `metadata`, but when dealing with edge chunks, which may overlap the true
#'   extent of the array, the returned array should be smaller than the chunk
#'   shape.
#'
#' @returns A list of length 2.  The first element is the formatted chunk data.
#'   The second is an integer of length 1, indicating if warnings were
#'   encountered when converting types
#'
#'   If "chunk_data" is larger than the space remaining in destination array
#'   i.e. it contains the overflowing elements, these will be trimmed when the
#'   chunk is returned to `read_data()`
#'
#' @keywords Internal
.format_chunk <- function(decompressed_chunk, metadata, alt_chunk_dim) {
  datatype <- .parse_datatype(metadata$dtype)
  
  ## It doesn't seem clear if the on disk chunk will contain the overflow 
  ## values or not, so we try both approaches.
  actual_chunk_size <- length(decompressed_chunk) / datatype$nbytes
  if(actual_chunk_size == prod(unlist(metadata$chunks))) {
    chunk_dim <- unlist(metadata$chunks)
  } else if (actual_chunk_size == prod(alt_chunk_dim)) {
    chunk_dim <- alt_chunk_dim
  } else {
    stop("Decompressed data doesn't match expected chunk size.")
  }
  
  ## reverse dimensions for column first datasets
  if (metadata$order == "C") {
    chunk_dim <- rev(chunk_dim)
  }

  if (datatype$base_type == "string") {
    ## break raw vector into list where each element is the bytes for 1 string
    tmp <- split(x = decompressed_chunk, 
                 f = rep(seq_len(length(decompressed_chunk) / datatype$nbytes),
      each = datatype$nbytes
    ))
    converted_chunk <- list(
      vapply(tmp, rawToChar, character(1), USE.NAMES = FALSE),
      0L ## no warning so set the second element to zero
    )
    dim(converted_chunk[[1]]) <- chunk_dim
  } else {
    output_type <- switch(datatype$base_type,
      "boolean" = 0L,
      "int" = 1L,
      "uint" = 1L,
      "float" = 2L
    )
    converted_chunk <- .Call("type_convert_chunk", decompressed_chunk,
      output_type, datatype$nbytes, datatype$is_signed,
      chunk_dim,
      PACKAGE = "Rarr"
    )
  }

  ## more manipulation to get the correct dimensions.
  ## Surely there's a way to do this in one step rather than two??
  if (metadata$order == "C") {
    converted_chunk[[1]] <- aperm(converted_chunk[[1]])
  }

  names(converted_chunk) <- c("chunk_data", "warning")
  return(converted_chunk)
}

#' Decompress a chunk in memory
#'
#' R has internal decompression tools for zlib, bz2 and lzma compression.  We
#' use external libraries bundled with the package for blosc and lz4
#' decompression.
#'
#' @param compressed_chunk Raw vector holding the compressed bytes for this
#'   chunk.
#' @param metadata List produced by `read_array_metadata()` with the contents of
#'   the `.zarray` file.
#'
#' @returns An array with the number of dimensions specified in the Zarr
#'   metadata.  In most cases it will have the same size as the Zarr chunk,
#'   however in the case of edge chunks, which overlap the extent of the array,
#'   the returned chunk will be smaller.
#'
#' @importFrom utils tail
#' @keywords Internal
.decompress_chunk <- function(compressed_chunk, metadata) {
  decompressor <- metadata$compressor$id
  datatype <- .parse_datatype(metadata$dtype)
  buffer_size <- get_chunk_size(datatype, dimensions = metadata$chunks)

  if(is.null(decompressor)) {
    decompressed_chunk <- compressed_chunk
  } else if (decompressor == "blosc") {
    decompressed_chunk <- .Call("decompress_chunk_BLOSC", compressed_chunk,
      PACKAGE = "Rarr"
    )
  } else if (decompressor %in% c("zlib", "gzip")) {
    decompressed_chunk <- memDecompress(
      from = compressed_chunk, type = "gzip",
      asChar = FALSE
    )
  } else if (decompressor == "bz2") {
    decompressed_chunk <- memDecompress(
      from = compressed_chunk, type = "bzip2",
      asChar = FALSE
    )
  } else if (decompressor == "lzma") {
    decompressed_chunk <- memDecompress(
      from = compressed_chunk, type = "xz",
      asChar = FALSE
    )
  } else if (decompressor == "lz4") {
    ## numpy codecs stores the original size of the buffer in the first 4 bytes
    decompressed_chunk <- .Call("decompress_chunk_LZ4",
      tail(x = compressed_chunk, n = -4L),
      buffer_size,
      PACKAGE = "Rarr"
    )
  } else {
    stop("Unsupported compression tool")
  }

  return(decompressed_chunk)
}
