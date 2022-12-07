#' @import jsonlite
read_metadata <- function(zarr_file) {
  metadata <- read_json(file.path(zarr_file, ".zarray"))
  return(metadata)
}

parse_datatype <- function(typestr) {
  
  datatype <- list()
  datatype_parts <- strsplit(typestr, "")[[1]]
  
  datatype$endian <- switch(datatype_parts[1],
                            "<" = "little",
                            ">" = "big",
                            "|" = NA)
  
  datatype$base_type <- switch(datatype_parts[2],
                               "b" = "logical",
                               "i" = "integer",
                               "u" = "uinteger",
                               "f" = "numeric",
                               "c" = "complex",
                               "m" = "timedelta",
                               "M" = "datetime",
                               "S" = "character",
                               "U" = "unicode",
                               "v" = "other")
  
  datatype$nbytes <- as.integer(datatype_parts[3])
  
  datatype$is_signed <- ifelse(datatype$base_type != "uinteger", TRUE, FALSE)
  
  return(datatype)
  
}