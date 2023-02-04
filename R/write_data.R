.check_datatype <- function(data_type, fill_value, nchar) {
  
  if(missing(data_type) && missing(fill_value)) {
    
    stop("Data type cannot be determined if both 'data_type' and 'fill_value' arguments are missing.")
    
  } else if(missing(data_type) && !missing(fill_value)) {
    ## if we only have a fill value, infer the data type from that
    data_type <- storage.mode(fill_value)
  } 
  
  ## if data type was supplied directly, always use that
  if(!data_type %in% c("<i4", "<f8", "|S")) {
    data_type <- switch(data_type,
                        "integer" = "<i4",
                        "double"  = "<f8",
                        "character" = "|S",
                        NULL)
  }
  
  if(is.null(data_type)) { stop("Currently only able to write integer, double, and character arrays") }
  
  ## set a default fill value if needed
  if(missing(fill_value)) {
    fill_value <- switch(data_type,
           "<i4" = 0L,
           "<f8" = 0,
           "|S"  = "",
           NULL)
  }
  
  if(data_type == "|S") {
    if(missing(nchar) || nchar < 1) {
      stop("The 'nchar' argument must be provided and be a positive integer")
    }
    data_type <- paste0("|S", as.integer(nchar))
  }
  

  
  return(list(data_type = data_type, fill_value = fill_value))
  
}


#' Create an (empty) Zarr array
#'
#'
#'
#' @param path Character vector of length 1 giving the path to the new Zarr
#'   array.
#' @param dim Dimensions of the new array.  Should be a numeric vector with the
#'   same length as the number of dimensions.
#' @param chunk_dim Dimensions of the array chunks. Should be a numeric vector
#'   with the same length as the `dim` argument.
#' @param data_type Character vector giving the data type of the new array.
#'   Currently this is limited to standard R data types.  Valid options are:
#'   "integer", "double", "character".  You can also use the analogous NumpPy
#'   formats: "<i4", "<f8", "|S".  If this argument isn't provided the
#'   `fill_value` will be used to determine the datatype.
#' @param compressor What (if any) compression tool should be applied to the
#'   array chunks.  The default is to use `zlib` compression.
#' @param fill_value The default value for uninitialized portions of the array.
#'   Does not have to be provided, in which case the default for the specified
#'   data type will be used.
#' @param nchar For `datatype = "character"` this parameter gives the maximum
#'   length of the stored strings. It is an error not to specify this for a
#'   character array, but it is ignored for other data types.
#'
#' @returns If successful returns (invisibly) `TRUE`.  However this function is
#'   primarily called for the size effect of initialising a Zarr array location
#'   and creating the `.zarray` metadata.
#'
#' @export
create_empty_zarr_array <- function(path, dim, chunk_dim, data_type, compressor = use_zlib(), fill_value, nchar, dimension_separator = ".") {
  
  path <- .normalize_array_path(path)
  if(!dir.exists(path)) { dir.create(path) }
  
  dt <- .check_datatype(data_type = data_type, fill_value = fill_value, nchar = nchar)
  data_type <- dt$data_type
  fill_value <- dt$fill_value
  
  .check_chunk_shape(x_dim = dim, chunk_dim = chunk_dim)
  
  .write_zarray(path = paste0(path, ".zarray"), 
                array_shape = dim, 
                chunk_shape = chunk_dim, 
                data_type = data_type,
                fill_value = fill_value,
                compressor = compressor)
  
  invisible(return(TRUE))
  
}

#' @export
write_zarr_array <- function(x, path, chunk_dim, compressor = use_zlib(), fill_value, nchar, dimension_separator = ".") {
  
  path <- .normalize_array_path(path)
  
  if(storage.mode(x) == "character" && missing(nchar)) { nchar = max(nchar(x)) }
  
  create_empty_zarr_array(path = path, dim = dim(x), chunk_dim = chunk_dim, 
                          data_type = storage.mode(x),
                          fill_value = fill_value, compressor = compressor,
                          nchar = nchar)
  
  chunk_names <- expand.grid(lapply(dim(x) %/% chunk_dim, seq_len)) - 1
  chunk_ids <- apply(chunk_names, 1, paste0, collapse = dimension_separator)
  
  ## iterate over each chunk
  res <- lapply(chunk_ids, FUN = .write_chunk, x = x, path = path, 
                chunk_dim = chunk_dim, compressor = compressor)
  
  invisible(return(all(unlist(res))))
}

.write_chunk <- function(chunk_id, x, path, chunk_dim, compressor) {
  
  chunk_id_split <- as.integer(strsplit(chunk_id, ".", fixed = TRUE)[[1]])
  chunk_path <- paste0(path, chunk_id)
  
  idx_in_array <- list(); 
  for(j in seq_along(dim(x))) { 
    idx_in_array[[j]] <- which((seq_len(dim(x)[j])-1) %/% chunk_dim[j] == chunk_id_split[j])
  }
  
  chunk_in_mem <- R.utils::extract(x, indices = idx_in_array)
  compressed_chunk <- .compress_chunk(input_chunk = chunk_in_mem, compressor = compressor)
  writeBin(compressed_chunk, con = chunk_path)
  invisible(return(TRUE))
}

#' Update the contents of an existing Zarr array
#' 
#' @param zarr_array_path 
#' @export
update_zarr_array <- function(zarr_array_path, x,  index) {
  
  stopifnot(is.list(index))
  
  metadata <- read_array_metadata(path)
  
  data_type <- switch(storage.mode(x),
                      "integer" = "<i4",
                      "double"  = "<f8",
                      "character" = "|S",
                      NULL)
  if(data_type != metadata$dtype) { stop("New data is not of the same type as the existing array.") }
  
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
      
      chunk_in_mem <- read_chunk(zarr_array_path = path, chunk_id = chunk_names[i,], metadata = metadata)[["chunk_data"]]
      chunk_in_mem <- .extract_and_replace(chunk_in_mem, idx_in_chunk, R.utils::extract(x, indices = idx_in_x))
      
      ## re-compress updated chunk and write back to disk
      compressed_chunk <- .compress_chunk(input_chunk = chunk_in_mem, compressor = metadata$compressor)
      writeBin(compressed_chunk, con = chunk_path)
    }
  }
  invisible(return(TRUE))
}

.compress_chunk <- function(input_chunk, compressor = use_zlib()) {
  
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