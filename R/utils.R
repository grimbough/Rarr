#' @keywords Internal
check_index <- function(index, metadata) {
  ## check we have the correct number of dimensions
  if (isFALSE(length(index) == length(metadata$shape))) {
    stop("The number of dimensions provided to 'index' does not match the shape of the array")
  }

  ## If any dimensions are NULL transform into the entirety of that dimension
  ## Otherwise check provided indices are valid
  failed <- rep(FALSE, n = length(index))
  for (i in seq_along(index)) {
    if (is.null(index[[i]])) {
      index[[i]] <- seq_len(metadata$shape[[i]])
    } else if (any(index[[i]] < 1) || any(index[[i]] > metadata$shape[[i]])) {
      failed[i] <- TRUE
    }
  }

  if (any(failed)) {
    stop(sprintf(
      "Selected indices for dimension(s) %s are out of range.",
      paste(which(failed), collapse = " & ")
    ))
  }

  return(index)
}

#' Create a string of the form `x[idx[[1]], idx[[2]]] <- y` for an array `x`
#' where the number of dimensions is variable.
#'
#' @param x_name Name of the object to have items replaced
#' @param idx_name Name of the list containing the indices
#' @param idx_length Length of the list specified in `idx_name`
#' @param y_name Name of the object containing the replacement items
#'
#' @returns A character vector of length one containing the replacement
#'   commands. This is expected to be passed to `parse() |> eval()`.
#'
#' @keywords Internal
.create_replace_call <- function(x_name, idx_name, idx_length, y_name) {

  args <- sprintf("%s[[%d]]", idx_name, seq_len(idx_length))
  args <- paste(args, collapse = ",")
  cmd <- sprintf("%s[%s] <- %s", x_name, args, y_name)
  
  return(cmd)
}

#' Parse the data type encoding string
#' 
#' @param typestr The datatype encoding string.  This is in the Numpy array
#' typestr format.
#' 
#' @returns A list of length 4 containing the details of the data type.
.parse_datatype <- function(typestr) {
  datatype <- list()
  datatype_parts <- strsplit(typestr, "")[[1]]

  datatype$endian <- switch(datatype_parts[1],
    "<" = "little",
    ">" = "big",
    "|" = NA
  )

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
    "V" = "other"
  )

  datatype$nbytes <- as.integer(datatype_parts[3])

  datatype$is_signed <- ifelse(datatype$base_type != "uint", TRUE, FALSE)

  return(datatype)
}

#' Parse the data type encoding string
#'
#' @param typestr The datatype encoding string.  This must be one of the values
#'   defined in the Zarr v3 specification
#'   (https://zarr-specs.readthedocs.io/en/latest/v3/core/v3.0.html#data-types)
#'
#' @returns A list of length 3 containing the details of the data type.
.parse_datatype_v3 <- function(typestr) {
  datatype <- list()

  datatype$base_type <- sub(x = typestr, pattern = "[0-9]*$", replacement = "")
  
  size <- sub(x = typestr, pattern = "^[a-z]*", replacement = "")
  ## bool is the only datatype that doesn't specify a size
  if(nchar(size) == 0)
    size = "1"
  datatype$nbytes <- as.integer(size) / 8
  
  datatype$is_signed <- ifelse(datatype$base_type != "uint", TRUE, FALSE)
  
  datatype$r_type <- switch(datatype$base_type,
                            "int" = "integer",
                            "uint" = "integer",
                            "float" = "numeric",
                            "bool" = "logical",
                            "raw")
  
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
  ## we strip the protocol because it gets messed up by the slash removal later
  if (grepl(x = path, pattern = "^((https?://)|(s3://)).*$")) {
    root <- gsub(x = path, pattern = "^((https?://)|(s3://)).*$", 
                 replacement = "\\1")
    path <- gsub(x = path, pattern = "^((https?://)|(s3://))(.*$)", 
                 replacement = "\\4")
  } else {
    ## Replace all backward slash ("\\") with forward slash ("/")
    path <- gsub(x = path, pattern = "\\", replacement = "/", fixed = TRUE)
    path <- normalizePath(path, winslash = "/", mustWork = FALSE)
    root <- gsub(x = path, "(^[[:alnum:]:.]*/)(.*)", replacement = "\\1")
    path <- gsub(x = path, "(^[[:alnum:]:.]*/)(.*)", replacement = "\\2")
  }

  ## Strip any leading "/" characters
  path <- gsub(x = path, pattern = "^/", replacement = "", fixed = FALSE)
  ## Strip any trailing "/" characters
  path <- gsub(x = path, pattern = "/$", replacement = "", fixed = FALSE)
  ## Collapse any sequence of more than one "/" character into a single "/" 
  path <- gsub(x = path, pattern = "//*", replacement = "/", fixed = FALSE)
  ## The key prefix is then obtained by appending a single "/" character to 
  ## the normalized logical path.
  path <- paste0(root, path, "/")

  return(path)
}
