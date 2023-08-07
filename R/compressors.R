#' Define compression tool and settings
#'
#' These functions select a compression tool and its setting when writing a Zarr
#' file
#' 
#' @param level Specify the compression level to use.
#'
#' @returns A list containing the details of the selected compression tool. This
#'   will be written to the .zarray metadata when the Zarr array is created.
#'   
#' @examples
#' 
#' ## define 2 compression filters for blosc (using snappy) and bzip2 (level 5)
#' blosc_with_snappy_compression <- use_blosc(cname = "snappy")
#' bzip2_compression <- use_bz2(level = 5)
#' 
#' ## create an example array to write to a file
#' x <- array(runif(n = 1000, min = -10, max = 10), dim = c(10, 20, 5))
#' 
#' ## write the array to two files using each compression filter
#' blosc_path <- tempfile()
#' bzip2_path <- tempfile()
#' write_zarr_array(
#'   x = x, zarr_array_path = blosc_path, chunk_dim = c(2, 5, 1), 
#'   compressor = blosc_with_snappy_compression
#' )
#' write_zarr_array(
#'   x = x, zarr_array_path = bzip2_path, chunk_dim = c(2, 5, 1), 
#'   compressor = bzip2_compression
#' )
#' 
#' ## the contents of the two arrays should be the same
#' identical(read_zarr_array(blosc_path), read_zarr_array(bzip2_path))
#' 
#' ## the size of the files on disk are not the same
#' sum(file.size(list.files(blosc_path, full.names = TRUE)))
#' sum(file.size(list.files(bzip2_path, full.names = TRUE)))
#'
#' @name compressors
NULL


#' @rdname compressors
#' 
#' @param cname Blosc is a 'meta-compressor' providing access to several
#'   compression algorithms.  This argument defines which compression tool
#'   should be used.  Valid options are: 'lz4', 'lz4hc', 'blosclz', 'zstd',
#'   'zlib', 'snappy'.
#' 
#' @export
use_blosc <- function(cname = "lz4") {
  
  valid_cname_options <- c("lz4", "lz4hc", "blosclz", "zstd", "zlib", "snappy")
  if(!tolower(cname) %in% valid_cname_options) {
    stop("'cname' argument must be one of '", 
         paste(valid_cname_options, collapse = "', '"), "'")
  }
  
  shuffle = "shuffle"
  valid_shuffle_options <- c("noshuffle", "shuffle", "bitshuffle")
  if(!tolower(shuffle) %in% valid_shuffle_options) {
    stop("'shuffle' argument must be one of '", 
         paste(valid_shuffle_options, collapse = "', '"), "'")
  }
  
  res <- list(id = "blosc", 
              cname = tolower(cname), 
              clevel = 5, 
              shuffle = as.integer(TRUE))
  return(res)
}

#' @rdname compressors
#' @export
use_zlib <- function(level = 6L) {
  res <- list(id = "zlib", level = as.integer(level))
  return(res)
}

#' @rdname compressors
#' @export
use_gzip <- function(level = 6L) {
  res <- list(id = "gzip", level = as.integer(level))
  return(res)
}

#' @rdname compressors
#' @export
use_bz2 <- function(level = 6L) {
  res <- list(id = "bz2", level = as.integer(level))
  return(res)
}

#' @rdname compressors
#' @export
use_lzma <- function(level = 9L) {
  res <- list(id = "lzma", format = 1, level = as.integer(level))
  return(res)
}

#' @rdname compressors
#' @export
use_lz4 <- function() {
  res <- list(id = "lz4", acceleration = 1)
  return(res)
}
