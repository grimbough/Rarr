#' @import jsonlite
#' @importFrom httr2 url_parse
#' @importFrom stringr str_extract str_remove
#' @importFrom aws.s3 s3read_using
#' 
#' @keywords Internal
read_array_metadata <- function(path, s3_provider = NULL) {
  
  zarray_path <- file.path(path, ".zarray")
  
  if(!is.null(s3_provider)) {
    if(s3_provider == "aws") {
      parsed_url <- url_parse_aws(zarray_path)
      metadata <- s3read_using(FUN = read_json, 
                               object = parsed_url$object, 
                               bucket = parsed_url$bucket, 
                               opts = list(region = parsed_url$region, 
                                           base_url = parsed_url$hostname))
      
    } else {
      parsed_url <- url_parse(zarray_path)
      bucket <- str_extract(parsed_url$path, pattern = "^/([[:alnum:]-]*)") |> 
        str_remove("/")
      object <- str_remove(string = parsed_url$path, pattern = "^/[[:alnum:]-_]*/")
      metadata <- s3read_using(FUN = read_json, 
                   object = object, 
                   bucket = bucket, 
                   opts = list(region = "", base_url = parsed_url$hostname))
    }
  } else {
    metadata <- read_json(file.path(path, ".zarray"))
  }
  
  metadata <- update_fill_value(metadata)
  
  return(metadata)
}

#' Special case fill values are encoded as strings.  R will create arrays of
#' type character if these are defined and the chunk isn't present on disk.
#' This function updates the fill value to be R's representation of these 
#' special values, so numeric arrays are created.
#' 
#' @keywords Internal
update_fill_value <- function(metadata) {
  
  if(metadata$fill_value %in% c("NaN", "Infinity", "-Infinity")) {
    datatype <- parse_datatype(metadata$dtype)
    if(datatype$base_type != "string") {
      metadata$fill_value <- switch(
        metadata$fill_value,
        "NaN" = NaN,
        "Infinity" = Inf,
        "-Infinity" = -Inf
      )
    }
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
  
  datatype$is_signed <- ifelse(datatype$base_type != "uint", TRUE, FALSE)
  
  return(datatype)
  
}