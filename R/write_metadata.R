

.write_zarray <- function(path, array_shape, chunk_shape, data_type, compressor) {
  
  zarray <- list()
  
  zarray$shape <- array_shape
  zarray$chunks <- chunk_shape
  zarray$dtype <- data_type
  zarray$compressor <- list(id = "zlib", level = "6")
  zarray$fill_value <- 0
  zarray$dimension_separator <- "."
  zarray$order <- "F"
  zarray$zarr_format <- 2
  
  ## weird hack
  zarray[ length(zarray)+1 ] <- list(NULL)
  names(zarray)[length(zarray)] <- "filters"
  
  json <- .format_json(toJSON(zarray, auto_unbox = TRUE, pretty = TRUE, null = "null"))
  write(x = json, file = path)
  
}

.format_json <- function(json) {
  
  json <- gsub(x = json, pattern = "[", replacement = "[\n    ", fixed = TRUE)
  json <- gsub(x = json, pattern = "],", replacement = "\n  ],", fixed = TRUE)
  json <- gsub(x = json, pattern = ", ", replacement = ",\n    ", fixed = TRUE)
  return(json)
}