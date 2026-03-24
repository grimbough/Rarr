#' @keywords internal
check_index <- function(index, metadata) {
  ## check we have the correct number of dimensions
  if (isFALSE(length(index) == length(metadata$shape))) {
    stop(
      "The number of dimensions provided to 'index' does not match the shape of the array"
    )
  }

  ## If any dimensions are NULL transform into the entirety of that dimension
  ## Otherwise check provided indices are valid
  failed <- rep_len(FALSE, length(index))
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

.create_chunk_names <- function(chunk_indices, metadata) {
  # In the DelayedArray framework, we can have integer(0) indices
  # https://github.com/Huber-group-EMBL/Rarr/issues/112.
  if (nrow(chunk_indices) == 0L) {
    return(character(0))
  }

  dim_separator <- metadata$chunk_key_encoding$configuration$separator %||% "/"

  # This is faster than vapply()
  chunk_names <- as.vector(apply(
    chunk_indices,
    1,
    paste,
    collapse = dim_separator
  ))

  if (metadata[["zarr_format"]] == 3) {
    if (identical(metadata[["shape"]], 1) && length(chunk_names) > 0) {
      chunk_names <- "c"
    } else {
      # In the DelayedArray framework, we can have integer(0) indices
      # https://github.com/Huber-group-EMBL/Rarr/issues/112
      chunk_names <- paste(
        "c",
        chunk_names,
        sep = dim_separator,
        recycle0 = TRUE
      )
    }
  }

  return(chunk_names)
}

#' Create a string of the form `x[idx[[1]], idx[[2]]] <- y` for an array `x`
#' where the number of dimensions is variable.
#'
#' @param x_name Name of the object to have items replaced
#' @param idx Indices
#' @param y_name Name of the object containing the replacement items
#'
#' @returns A character vector of length one containing the replacement
#'   commands. This is expected to be passed to `parse() |> eval()`.
#'
#' @keywords internal
.create_replace_call <- function(x_name, idx, y_name) {
  args <- paste(idx, collapse = ",")
  cmd <- sprintf("%s[%s] <- %s", x_name, args, y_name)

  return(cmd)
}

.create_extract_call <- function(x_name, idx) {
  args <- paste(c(idx, "drop=FALSE"), collapse = ",")
  cmd <- sprintf("%s[%s]", x_name, args)

  return(cmd)
}

#' Parse the data type encoding string
#'
#' @param typestr The datatype encoding string.  This is in the Numpy array
#' typestr format.
#'
#' @returns A list of length 4 containing the details of the data type.
#'
#' @keywords internal
.parse_datatype <- function(typestr) {
  # structured data type
  if (is.list(typestr)) {
    types <- lapply(typestr, function(field) .parse_datatype(field[[2]]))
    types$nbytes <- vapply(types, function(x) x$nbytes, integer(1))
    types$base_type <- "structured"
    return(types)
  }

  datatype <- list()
  datatype_parts <- strsplit(typestr, "", fixed = TRUE)[[1]]

  datatype$endian <- switch(
    datatype_parts[1],
    "<" = "little",
    ">" = "big",
    "|" = NA
  )

  datatype$base_type <- switch(
    datatype_parts[2],
    "b" = "bool",
    "i" = "int",
    "u" = "uint",
    "f" = "float",
    "c" = "complex",
    "m" = "timedelta",
    "M" = "datetime",
    "S" = "string",
    "U" = "unicode",
    "V" = "other",
    "O" = "py_object"
  )

  datatype$nbytes <- as.integer(
    gsub(x = typestr, pattern = "^[<>|][[:alpha:]]", replacement = "")
  )

  if (datatype$base_type == "unicode") {
    datatype$nbytes <- datatype$nbytes * 4L
  }

  return(datatype)
}

.parse_datatype_v3 <- function(typestr) {
  if (is.list(typestr)) {
    stop(
      "Only base data types (not extensions) are supported for Zarr v3 arrays for now",
      call. = FALSE
    )
  }
  datatype <- list()

  datatype$base_type <- gsub("^([[:alpha:]]+).*", "\\1", typestr)

  # FIXME: it's awkward to have to reconvert to integer after the division
  datatype$nbytes <- as.integer(
    as.integer(
      gsub(x = typestr, pattern = "^[^[:digit:]]+", replacement = "")
    ) /
      8L
  )
  if (is.na(datatype$nbytes)) {
    datatype$nbytes <- 1L
  }

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
#' @keywords internal
.normalize_array_path <- function(path) {
  ## we strip the protocol because it gets messed up by the slash removal later
  if (grepl(x = path, pattern = "^((https?://)|(s3://)).*$")) {
    root <- gsub(
      x = path,
      pattern = "^((https?://)|(s3://)).*$",
      replacement = "\\1"
    )
    path <- gsub(
      x = path,
      pattern = "^((https?://)|(s3://))(.*$)",
      replacement = "\\4"
    )
  } else {
    ## Replace all backward slash ("\\") with forward slash ("/")
    path <- gsub(x = path, pattern = "\\", replacement = "/", fixed = TRUE)
    path <- R.utils::getAbsolutePath(path, expandTilde = TRUE)
    root <- gsub(x = path, "(^[[:alnum:]:.]*/)?(.*)", replacement = "\\1")
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

#' @importFrom stats setNames
.file_or_blob_exists <- function(
  zarr_array_path,
  s3_client,
  files
) {
  if (is.null(s3_client)) {
    is_present <- setNames(
      file.exists(paste0(zarr_array_path, files, recycle0 = TRUE)),
      files
    )
  } else {
    parse_url <- parse_s3_path(zarr_array_path)
    is_present <- vapply(
      files,
      FUN = function(f) {
        key <- paste0(parse_url$object, f)
        .s3_object_exists(s3_client, parse_url$bucket, key)
      },
      FUN.VALUE = logical(1)
    )
  }

  return(is_present)
}
