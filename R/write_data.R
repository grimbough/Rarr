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
#' @param zarr_array_path Character vector of length 1 giving the path to the new Zarr
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
#' @param dimension_separator The character used to to separate the dimensions
#'   in the names of the chunk files.  Valid options are limited to "." and "/".
#'
#' @returns If successful returns (invisibly) `TRUE`.  However this function is
#'   primarily called for the size effect of initialising a Zarr array location
#'   and creating the `.zarray` metadata.
#'   
#' @seealso [write_zarr_array()], [update_zarr_array()]
#' 
#' @examples 
#' 
#' new_zarr_array <- file.path(tempdir(), "temp.zarr")
#' create_empty_zarr_array(new_zarr_array, dim = c(10,20), chunk_dim = c(2,5), 
#'                         data_type = "integer")
#'
#' @export
create_empty_zarr_array <- function(zarr_array_path, dim, chunk_dim, data_type, 
                                    compressor = use_zlib(), fill_value, nchar, 
                                    dimension_separator = ".") {
  
  path <- .normalize_array_path(zarr_array_path)
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

#' Write an R array to Zarr
#'
#' @param x The R array (or object that can be coerced to an array) that will be
#'   written to the Zarr array.
#' @param zarr_array_path Character vector of length 1 giving the path to the
#'   new Zarr array.
#' @param chunk_dim Dimensions of the array chunks. Should be a numeric vector
#'   with the same length as the `dim` argument.
#' @param compressor What (if any) compression tool should be applied to the
#'   array chunks.  The default is to use `zlib` compression.
#' @param fill_value The default value for uninitialized portions of the array.
#'   Does not have to be provided, in which case the default for the specified
#'   data type will be used.
#' @param nchar For character arrays this parameter gives the maximum length of
#'   the stored strings. If this argument is not specified the array provided to
#'   `x` will be checked and the length of the longest string found will be used
#'   so no data are truncated. However this may be slow and providing a value to
#'   `nchar` can provide a modest performance improvement.
#' @param dimension_separator The character used to to separate the dimensions
#'   in the names of the chunk files.  Valid options are limited to "." and "/".
#'
#' @returns The function is primarily called for the side effect of writing to
#'   disk. Returns (invisibly) `TRUE` if the array is successfully written.
#'
#' @examples
#'
#' new_zarr_array <- file.path(tempdir(), "integer.zarr")
#' x <- array(1:50, dim = c(10,5))
#' write_zarr_array(x = x, zarr_array_path = new_zarr_array,
#'                 chunk_dim = c(2,5))
#'
#' @export
write_zarr_array <- function(x, zarr_array_path, chunk_dim, compressor = use_zlib(), fill_value, nchar, dimension_separator = ".") {
  
  path <- .normalize_array_path(zarr_array_path)
  
  if(storage.mode(x) == "character" && missing(nchar)) { nchar <- max(nchar(x)) }
  
  create_empty_zarr_array(zarr_array_path = path, dim = dim(x), chunk_dim = chunk_dim, 
                          data_type = storage.mode(x),
                          fill_value = fill_value, compressor = compressor,
                          nchar = nchar)
  
  chunk_names <- expand.grid(lapply(dim(x) %/% chunk_dim, seq_len)) - 1
  chunk_ids <- apply(chunk_names, 1, paste0, collapse = dimension_separator)
  
  ## iterate over each chunk
  ## TODO: maybe this can be done in parallel is bplapply() ?
  res <- lapply(chunk_ids, FUN = .write_chunk, x = x, path = path, 
                chunk_dim = chunk_dim, compressor = compressor)
  
  return(invisible(all(unlist(res))))
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
  return(invisible(TRUE))
}

#' Update (a subset of) an existing Zarr array
#'
#' @param zarr_array_path Character vector of length 1 giving the path to the
#'   Zarr array that is to be modified.
#' @param x The R array (or object that can be coerced to an array) that will be
#'   written to the Zarr array.
#' @param index A list with the same length as the number of dimensions of the
#'   target array. This argument indicates which elements in the target array
#'   should be updated.
#'
#' @returns The function is primarily called for the side effect of writing to
#'   disk. Returns (invisibly) `TRUE` if the array is successfully updated.
#'
#' @examples
#' 
#' ## first create a new, empty, Zarr array
#' new_zarry_array <- file.path(tempdir(), "new_array.zarr")
#' create_empty_zarr_array(zarr_array_path = new_zarry_array, dim = c(20, 10),
#'                         chunk_dim = c(10, 5), data_type = "double")
#'                         
#' ## create a matrix smaller than our Zarr array
#' small_matrix <- matrix(runif(6), nrow = 3)
#'
#' ## insert the matrix into the first 3 rows, 2 columns of the Zarr array
#' update_zarr_array(new_zarry_array, x = small_matrix, index = list(1:3, 1:2))
#'
#' ## reading back a slightly larger subset, 
#' ## we can see only the top left corner has been changed
#' read_zarr_array(new_zarry_array, index = list(1:5, 1:5))
#'
#' @export
update_zarr_array <- function(zarr_array_path, x,  index) {
  
  stopifnot(is.list(index))
  
  zarr_array_path <- .normalize_array_path(zarr_array_path)
  metadata <- read_array_metadata(zarr_array_path)
  
  data_type <- switch(storage.mode(x),
                      "integer" = "<i4",
                      "double"  = "<f8",
                      "character" = "|S",
                      NULL)
  if(data_type != metadata$dtype) { stop("New data is not of the same type as the existing array.") }
  
  zarr_dim <- unlist(metadata$shape)
  chunk_dim <- unlist(metadata$chunks)
  
  ## coerce x to the same shape as the zarr to be updated
  x <- array(x, dim = vapply(index, length, integer(1)))
  
  ## create all possible chunk names, then remove those that won't be touched
  chunk_names <- expand.grid(lapply(zarr_dim %/% chunk_dim, seq_len)) - 1
  chunk_needed <- rep(FALSE, nrow(chunk_names))
  for(i in seq_len(nrow(chunk_names))) {
    idx_in_zarr <- list()
    for(j in seq_along(zarr_dim)) { 
      idx_in_zarr[[j]] <- index[[j]][ which((index[[j]]-1) %/% chunk_dim[j] == chunk_names[i,j]) ]
    }
    chunk_needed[i] <- all(lengths(idx_in_zarr) > 0)
  }
  chunk_names <- chunk_names[chunk_needed, , drop = FALSE]
  chunk_ids <- apply(chunk_names, 1, paste0, collapse = metadata$dimension_separator)
  
  ## only update the chunks that need to be
  ## TODO: maybe this can be done in parallel is bplapply() ?
  res <- lapply(chunk_ids, FUN = .update_chunk, x = x, path = zarr_array_path, 
                chunk_dim = chunk_dim, index = index,
                metadata = metadata)
  
  return(invisible(all(unlist(res))))
}

.update_chunk <- function(chunk_id, x, path, chunk_dim, index, 
                          metadata) {
  
  chunk_id_split <- as.integer(
    strsplit(chunk_id, metadata$dimension_separator, 
             fixed = TRUE)[[1]]
  )
  chunk_path <- paste0(path, chunk_id)
  
  ## determine which elements of x are being used and where in this specific
  ## chunk they should be inserted
  ## TODO: This is pretty ugly, maybe there's something more elegant
  idx_in_zarr <- idx_in_x <- idx_in_chunk <- list(); 
  for(j in seq_along(chunk_dim)) { 
    idx_in_zarr[[j]]  <- index[[j]][ which((index[[j]]-1) %/% chunk_dim[j] == chunk_id_split[j]) ]
    idx_in_x[[j]]     <- which((index[[j]]-1) %/% chunk_dim[j] == chunk_id_split[j])
    idx_in_chunk[[j]] <- ((idx_in_zarr[[j]]-1) %% chunk_dim[j])+1
  }
  
  chunk_in_mem <- read_chunk(zarr_array_path = path, 
                             chunk_id = chunk_id_split, 
                             metadata = metadata)[["chunk_data"]]
  chunk_in_mem <- .extract_and_replace(chunk_in_mem, idx_in_chunk, 
                                       R.utils::extract(x, indices = idx_in_x))
  
  ## re-compress updated chunk and write back to disk
  compressed_chunk <- .compress_chunk(input_chunk = chunk_in_mem, 
                                      compressor = metadata$compressor)
  writeBin(compressed_chunk, con = chunk_path)
}

.compress_chunk <- function(input_chunk, compressor = use_zlib()) {
  
  ## the compression tools need a raw vector
  ## any permuting for "C" ordering needs to happen before this
  raw_chunk <- .as_raw(as.vector(input_chunk))
  
  if(compressor$id == "blosc") {
    compressed_chunk <- .Call("compress_chunk_BLOSC", raw_chunk, PACKAGE = "Rarr")
  } else if (compressor$id %in% c("zlib")) {
    compressed_chunk <- memCompress(from = raw_chunk, type = "gzip")
  } else if (compressor$id == "bz2") {
    compressed_chunk <- memCompress(from = raw_chunk, type = "bzip2")
  } else if (compressor$id == "lzma") {
    compressed_chunk <- memCompress(from = raw_chunk, type = "xz")
    #} else if (compressor$id == "lz4") {
    ## TODO: Implement LZ4 compression
    ## numpy stores the original size of the buffer in the first 4 bytes after
    ## compression. We should do that too for compatibility
    #  compressed_chunk <- .Call("compress_chunk_LZ4", raw_chunk, PACKAGE = "Rarr")
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
  
  return(invisible(TRUE))
  
}