
.format_string <- function(decompressed_chunk, datatype) {
  ## break raw vector into list where each element is the bytes for 1 string
  tmp <- split(x = decompressed_chunk, 
               f = ceiling(seq_along(decompressed_chunk) / datatype$nbytes))
  converted_chunk <- list(
    vapply(tmp, rawToChar, character(1), USE.NAMES = FALSE),
    0L ## no warning so set the second element to zero
  )
  return(converted_chunk)
}

.format_unicode <- function(decompressed_chunk, datatype) {
  tmp <- split(x = decompressed_chunk,
               f = ceiling(seq_along(decompressed_chunk) / datatype$nbytes))
  converted_chunk <- list(
    vapply(tmp, FUN = function(x) {
      intToUtf8(readBin(x, what = "integer", size = 4, n = length(x)/4))
    }, FUN.VALUE = character(1), USE.NAMES = FALSE),
    0L
  )
  return(converted_chunk)
}

## for now we're going to assume you only get here with a VLEN UTF8 datatype
.format_object <- function(decompressed_chunk, metadata, datatype) {
  
  if(length(metadata$filters) != 1) {
    stop("Unknown object data type")
  }
  
  if(metadata$filters[[1]]$id != "vlen-utf8") {
    stop("Not VLEN UTF8 encoded data.  We don't know how to process this!")
  }
  
  converted_chunk <- list(
    .readVlenUTF8(decompressed_chunk), 
    0L
  )
  return(converted_chunk)
}
