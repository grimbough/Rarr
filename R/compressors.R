#' Define compression tool and settings
#'
#' These functions select a compression tool and its setting when writing a Zarr
#' file
#'
#' @returns A list containing the details of the selected compression tool. This
#'   will be written to the .zarray metadata when the Zarr array is created.
#'
#' @name compressors
NULL


#' @rdname compressors
#' 
#' @param cname Blosc is a 'meta-compressor' providing access to several
#'   compression algorithms.  This argument defines which compression tool
#'   should be used.  Valid options are: 'lz4', 'lz4hc', 'blosclz', 'zstd',
#'   'zlib', 'snappy'
#' 
#' @export
use_blosc <- function(cname = "lz4") {
  
  valid_options <- c("lz4", "lz4hc", "blosclz", "zstd", "zlib", "snappy")
  if(!tolower(cname) %in% valid_options) {
    stop("'cname argument must be one of '", 
         paste(valid_options, collapse = "', '"), "'")
  }
  
  res <- list(id = "blosc", cname = tolower(cname), 
              clevel = 5, shuffle = as.integer(TRUE))
  return(res)
}

#' @rdname compressors
#' @export
use_zlib <- function() {
  res <- list(id = "zlib", level = 6)
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
use_lzma <- function() {
  res <- list(id = "lzma", format = 1, level = 9)
  return(res)
}

#' @rdname compressors
#' @export
use_lz4 <- function() {
  res <- list(id = "lz4", acceleration = 1)
  return(res)
}
