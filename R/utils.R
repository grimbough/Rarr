#' @keywords internal
check_index <- function(index, metadata) {
  index_len <- length(index)
  shape <- metadata$shape
  ## check we have the correct number of dimensions
  if (index_len != length(shape)) {
    stop(
      "The number of dimensions provided to 'index' does not match the shape of the array"
    )
  }

  ## If any dimensions are NULL transform into the entirety of that dimension
  ## Otherwise check provided indices are valid
  failed <- rep_len(FALSE, index_len)
  for (i in seq_len(index_len)) {
    if (is.null(index[[i]])) {
      index[[i]] <- seq_len(shape[[i]])
    } else if (
      length(index[[i]]) > 0L &&
        (min(index[[i]]) < 1L || max(index[[i]]) > shape[[i]])
    ) {
      failed[i] <- TRUE
    } else {
      index[[i]] <- as.integer(index[[i]])
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

#' Subset extraction for an array with a variable number of dimensions.
#'
#' @param x Array to extract from.
#' @param idx List of index vectors, one per dimension.
#'
#' @returns The extracted sub-array (with `drop = FALSE`).
#'
#' @keywords internal
.extract_chunk <- function(x, idx) {
  # do.call() has the same performance if we ever need to drop rlang dependency
  # but this is more aesthetically pleasing and rlang is likely to always be
  # somewhere in the dependency tree.
  rlang::inject(x[!!!idx, drop = FALSE])
}

.parse_datatype_v3 <- function(typestr) {
  if (is.list(typestr)) {
    if (typestr$name == "fixed_length_utf32") {
      return(list(
        base_type = "unicode",
        nbytes = typestr$configuration$length_bytes
      ))
    }
    if (typestr$name == "null_terminated_bytes") {
      return(list(
        base_type = "string",
        nbytes = typestr$configuration$length_bytes
      ))
    }
    if (typestr$name == "struct") {
      internal_types <- lapply(typestr$configuration$fields, function(field) {
        .parse_datatype_v3(field$data_type)
      })
      return(
        list(
          base_type = vapply(
            internal_types,
            `[[`,
            "base_type",
            FUN.VALUE = character(1L)
          ),
          nbytes = vapply(
            internal_types,
            `[[`,
            "nbytes",
            FUN.VALUE = integer(1L)
          )
        )
      )
    }
    if (typestr$name == "structured") {
      internal_types <- lapply(typestr$configuration$fields, function(field) {
        .parse_datatype_v3(field[[2L]])
      })
      return(
        list(
          base_type = vapply(
            internal_types,
            `[[`,
            "base_type",
            FUN.VALUE = character(1L)
          ),
          nbytes = vapply(
            internal_types,
            `[[`,
            "nbytes",
            FUN.VALUE = integer(1L)
          )
        )
      )
    }
    # nocov start
    stop("Unsupported data type: ", typestr$name, call. = FALSE)
    # nocov end
  }

  entry <- SUPPORTED_V3_TYPES[[typestr]]
  if (is.null(entry)) {
    # nocov start
    stop("Unsupported data type: ", typestr, call. = FALSE)
    # nocov end
  }
  return(list(
    base_type = entry$base_type,
    nbytes = entry$nbytes
  ))
}
