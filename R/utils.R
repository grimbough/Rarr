#' @keywords Internal
check_index <- function(index, metadata) {
  
  ## check we have the correct number of dimensions
  if(isFALSE(length(index) == length(metadata$shape))) {
    stop("The number of dimensions provided to 'index' does not match the shape of the array")
  }
  
  ## If any dimensions are NULL transform into the entirety of that dimension
  ## Otherwise check provided indices are valid
  failed <- rep(FALSE, n = length(index))
  for(i in seq_along(index)) {
    if (is.null(index[[i]])) {
      index[[i]] <- seq_len(metadata$shape[[i]])
    } else if(any(index[[i]] < 1) || any(index[[i]] > metadata$shape[[i]])) {
      failed[i] <- TRUE
    }
  }
  
  if(any(failed)) {
    stop(sprintf("Selected indices for dimension(s) %s are out of range.", 
                 paste(which(failed), collapse = " & ")))
  }
  
  return(index)
  
}

s3_provider <- function(path) {
  
  if(!grepl(pattern = "(^https?://)|(^s3://)", x = path)) {
    provider <- NULL
  } else {
    matches <- regmatches(x = path, 
                          m = regexpr(pattern = "(amazonaws\\.com)|(embl\\.de)", 
                                      text = path))
    if(!length(matches)) { matches <- "other" }
    provider <- switch(matches,
                       "amazonaws.com" = "aws",
                       "embl.de"       = "other",
                       "other")
  }
  return(provider)
}

#' @importFrom httr2 url_parse
#' @keywords Internal
url_parse_aws <- function(url) {
  
  tmp <- httr2::url_parse(url)
  
  if(grepl(pattern = "^https?://s3\\.", x = url, ignore.case = TRUE)) {
    ## path style address
    bucket <- gsub(x = tmp$path, pattern = "^/([a-z0-9\\.-]*)/.*", replacement = "\\1", ignore.case = TRUE)
    object <- gsub(x = tmp$path, pattern = "^/([a-z0-9\\.-]*)/(.*)", replacement = "\\2", ignore.case = TRUE)
    region <- gsub(x = url, pattern = "^https?://s3\\.([a-z0-9-]*)\\.amazonaws\\.com/.*$", replacement = "\\1")
  } else if (grepl(pattern = "^https?://[a-z0-9\\.-]*.s3\\.", x = url, ignore.case = TRUE)) {
    ## virtual-host style address
    bucket <- gsub(x = tmp$hostname, pattern = "^([a-z0-9\\.-]*)\\.s3.*", replacement = "\\1", ignore.case = TRUE)
    object <- tmp$path
    region <- gsub(x = tmp$hostname, pattern = "^.*\\.s3\\.([a-z0-9-]*)\\.amazonaws\\.com$", replacement = "\\1", ignore.case = TRUE)
  } else {
    stop("Unknown AWS path style.  Please report this to the package maintainer.")
  }
  
  res <- list(bucket = bucket, object = object, region = region, hostname = "s3.amazonaws.com")  
  return(res)
}

#' @keywords Internal
.url_parse_other <- function(url) {
  parsed_url <- url_parse(url)
  bucket <- gsub(x = parsed_url$path, pattern = "^/([[a-z0-9\\.-]*)/.*", replacement = "\\1", ignore.case = TRUE)
  object <- gsub(x = parsed_url$path, pattern = "^/([a-z0-9\\.-]*)/(.*)", replacement = "\\2", ignore.case = TRUE)
  res <- list(bucket = bucket, object = object, region = "", hostname = parsed_url$hostname)  
  return(res)
}

.extract_and_replace <- function(x, indices, y) {
  
  dims <- seq_along(indices)
  args <- rep("", times=length(indices))
  for (kk in seq_along(indices)) {
    dd <- dims[kk]
    args[dd] <- sprintf("indices[[%d]]", kk)
  }
  args <- paste(args, collapse=",")
  code <- paste("x[", args, "] <- y", sep="")
  
  eval(parse(text = code))
  return(x)
  
}

.parse_datatype <- function(typestr) {
  
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


#' Normalize a Zarr array path
#' 
#' Taken from https://zarr.readthedocs.io/en/stable/spec/v2.html#logical-storage-paths
#' 
#' @param path Character vector of length 1 giving the path to be normalised.
#' 
#' @returns A character vector of length 1 containing the normalised path.
#' 
#' @keywords Internal
.normalize_array_path <- function(path) {
  
  ## we strip the protocol because it gets messed up by the slash removal later on
  if(grepl(x = path, pattern = "^((https?://)|(s3://)).*$"))
    protocol <- gsub(x = path, pattern = "^((https?://)|(s3://)).*$", replacement = "\\1")
  else 
    protocol <- "/"
  path <- gsub(x = path, pattern = "^((https?://)|(s3://))(.*$)", replacement = "\\4")
  
  ## Replace all backward slash characters ("\\") with forward slash characters ("/")
  path <- gsub( x = path, pattern = "\\", replacement = "/", fixed = TRUE)
  ## Strip any leading "/" characters
  path <- gsub( x = path, pattern = "^/", replacement = "", fixed = FALSE)
  ## Strip any trailing "/" characters
  path <- gsub( x = path, pattern = "/$", replacement = "", fixed = FALSE)
  ## Collapse any sequence of more than one “/” character into a single “/” character
  path <- gsub( x = path, pattern = "//*", replacement = "/", fixed = FALSE)
  ## The key prefix is then obtained by appending a single “/” character to the normalized logical path.
  path <- paste0(protocol, path, "/")
  
  return(path)
}