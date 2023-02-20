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

.extract_and_replace <- function(x, indices, y) {
  dims <- seq_along(indices)
  args <- rep("", times = length(indices))
  for (kk in seq_along(indices)) {
    dd <- dims[kk]
    args[dd] <- sprintf("indices[[%d]]", kk)
  }
  args <- paste(args, collapse = ",")
  code <- paste("x[", args, "] <- y", sep = "")

  eval(parse(text = code))
  return(x)
}

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
    "v" = "other"
  )

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
  if (grepl(x = path, pattern = "^((https?://)|(s3://)).*$")) {
    root <- gsub(x = path, pattern = "^((https?://)|(s3://)).*$", replacement = "\\1")
    path <- gsub(x = path, pattern = "^((https?://)|(s3://))(.*$)", replacement = "\\4")
  } else {
    ## Replace all backward slash characters ("\\") with forward slash characters ("/")
    path <- gsub(x = path, pattern = "\\", replacement = "/", fixed = TRUE)
    path <- normalizePath(path, winslash = "/", mustWork = FALSE)
    root <- gsub(x = path, "(^[[:alnum:]:.]*/)(.*)", replacement = "\\1")
    path <- gsub(x = path, "(^[[:alnum:]:.]*/)(.*)", replacement = "\\2")
  }

  ## Strip any leading "/" characters
  path <- gsub(x = path, pattern = "^/", replacement = "", fixed = FALSE)
  ## Strip any trailing "/" characters
  path <- gsub(x = path, pattern = "/$", replacement = "", fixed = FALSE)
  ## Collapse any sequence of more than one “/” character into a single “/” character
  path <- gsub(x = path, pattern = "//*", replacement = "/", fixed = FALSE)
  ## The key prefix is then obtained by appending a single “/” character to the normalized logical path.
  path <- paste0(root, path, "/")

  return(path)
}
