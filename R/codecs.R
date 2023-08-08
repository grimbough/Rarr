.check_blosc_args <- function(cname, clevel, shuffle) {
  
  valid_cname_options <- c("lz4", "lz4hc", "blosclz", "zstd", "zlib", "snappy")
  if(!tolower(cname) %in% valid_cname_options) {
    stop("'cname' argument must be one of '", 
         paste(valid_cname_options, collapse = "', '"), "'")
  }
  
  clevel <- as.integer(clevel)
  if(clevel < 0L || clevel > 9L)
    stop("'clevel' argument must be between 0 and 9")
  
  shuffle = "shuffle"
  valid_shuffle_options <- c("noshuffle", "shuffle", "bitshuffle")
  if(!tolower(shuffle) %in% valid_shuffle_options) {
    stop("'shuffle' argument must be one of '", 
         paste(valid_shuffle_options, collapse = "', '"), "'")
  }
  
  return(invisible(TRUE))
  
}

blosc_codec <- function(chunk, cname, clevel, shuffle, encoding = FALSE) {
  
  .check_blosc_args(cname, clevel, shuffle)
  
  if(encoding) {
    
  } else {
    output <- .Call("decompress_chunk_BLOSC", chunk, PACKAGE = "Rarr")
  }
  
  return(output)
}

endian_codec <- function(chunk, endian, data_type, encoding = FALSE) {
  
  endian <- tolower(endian)
  stopifnot(endian %in% c("big", "little"))
  
  if(endian != .Platform$endian) {
    stop("Rarr currently only supports using the same endianness as the current machine")
  }
  
  if(encoding) {
    
  } else {
    
    if(data_type$r_type %in% c("integer", "numeric")) {
      output <- readBin(con = chunk, 
                        what = data_type$r_type,
                        n = length(chunk), 
                        size = data_type$nbytes,
                        signed = data_type$is_signed,
                        endian = endian)
    }
    
  }
  
  return(output)
}

transpose_codec <- function(chunk, chunk_dim, order, encoding = FALSE) {
  
  if(encoding) {
    
  } else {
    if(order == "C") {
      dim(chunk) <- rev(chunk_dim)
      chunk <- aperm(chunk)
    } else if(order == "F") {
      dim(chunk) <- chunk_dim
    } else {
      stop("Only 'C' and 'F' chunk ordering are currently supported.")
    }
  }
  return(chunk)
}

gzip_codec <- function(chunk, level = 5, encoding = FALSE) {
  
  if(encoding) {
    
  } else {
    output <- memDecompress(
      from = chunk, 
      type = "gzip",
      asChar = FALSE
    )
  }
  return(output)
}

