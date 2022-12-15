#' @import jsonlite
#' @importFrom httr2 url_parse
#' @importFrom stringr str_extract str_remove
#' @importFrom aws.s3 s3read_using
read_array_metadata <- function(path, is_s3 = FALSE) {
  
  zarray_path <- file.path(path, ".zarray")
  
  if(is_s3) {
    parsed_url <- url_parse(zarray_path)
    bucket <- str_extract(parsed_url$path, pattern = "^/([[:alnum:]-]*)") |> 
      str_remove("/")
    object <- str_remove(string = parsed_url$path, pattern = "^/[[:alnum:]-_]*/")
    metadata <- s3read_using(FUN = read_json, 
                 object = object, 
                 bucket = bucket, 
                 opts = list(region = "", base_url = parsed_url$hostname))
  } else {
    metadata <- read_json(file.path(path, ".zarray"))
  }
  return(metadata)
}

#' @import jsonlite
read_zarr_metadata <- function(zarr_file) {
  
  archive_metadata <- file.path(zarr_file, ".zmetadata")
  if(file.exists(archive_metadata)) {
    metadata <- read_json(file.path(zarr_file, ".zmetadata"))
  } else {
    metadata <- NULL
  }
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
                               "b" = "boolean",
                               "i" = "int",
                               "u" = "uint",
                               "f" = "float",
                               "c" = "complex",
                               "m" = "timedelta",
                               "M" = "datetime",
                               "S" = "string",
                               "U" = "Unicode",
                               "v" = "other")
  
  datatype$nbytes <- as.integer(datatype_parts[3])
  
  datatype$is_signed <- ifelse(datatype$base_type != "uinteger", TRUE, FALSE)
  
  return(datatype)
  
}