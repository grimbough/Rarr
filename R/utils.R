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
    } else if (any(index[[i]] < 1L) || any(index[[i]] > metadata$shape[[i]])) {
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
    return(character(0L))
  }

  dim_separator <- metadata$chunk_key_encoding$configuration$separator %||% "/"

  # This is faster than vapply()
  chunk_names <- as.vector(apply(
    chunk_indices,
    1L,
    paste,
    collapse = dim_separator
  ))

  if (metadata[["zarr_format"]] == 3L) {
    if (identical(metadata[["shape"]], 1L) && length(chunk_names) > 0L) {
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
#' @param idx_name Name of the list containing the indices
#' @param idx_length Length of the list specified in `idx_name`
#' @param y_name Name of the object containing the replacement items
#'
#' @returns A character vector of length one containing the replacement
#'   commands. This is expected to be passed to `parse() |> eval()`.
#'
#' @keywords internal
.create_replace_call <- function(x_name, idx_name, idx_length, y_name) {
  # This is ugly but AFAICT this is the only way that doesn't require
  # a copy of `x`.
  args <- sprintf("%s[[%d]]", idx_name, seq_len(idx_length))
  args <- paste(args, collapse = ",")
  cmd <- sprintf("%s[%s] <- %s", x_name, args, y_name)

  return(cmd)
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
    stop("Unsupported data type: ", typestr$name, call. = FALSE)
  }

  entry <- supported_v3_types[[typestr]]
  if (is.null(entry)) {
    stop("Unsupported data type: ", typestr, call. = FALSE)
  }
  return(list(
    base_type = entry$base_type,
    nbytes = entry$nbytes
  ))
}


#' Normalize a Zarr array path
#'
#' Taken from https://zarr.readthedocs.io/en/stable/spec/v2.html#logical-storage-paths
#'
#' @param path Character vector of length 1 giving the path to be normalised.
#'
#' @returns A character vector of length 1 containing the normalised path.
#'
#' @importFrom R.utils getAbsolutePath
#'
#' @keywords internal
.normalize_array_path <- function(path) {
  ## we strip the protocol because it gets messed up by the slash removal later
  if (any(startsWith(path, c("http://", "https://", "s3://")))) {
    m <- regmatches(path, regexec("^((https?://)|(s3://))(.*$)", path))[[1L]]
    root <- m[2L]
    path <- m[5L]
  } else {
    ## Replace all backward slash ("\\") with forward slash ("/")
    path <- gsub(x = path, pattern = "\\", replacement = "/", fixed = TRUE)
    path <- R.utils::getAbsolutePath(path, expandTilde = TRUE)
    root <- sub(x = path, "(^[[:alnum:]:.]*/)?(.*)", replacement = "\\1")
    path <- sub(x = path, "(^[[:alnum:]:.]*/)(.*)", replacement = "\\2")
  }

  ## Strip any leading "/" characters
  path <- sub(x = path, pattern = "^/", replacement = "", fixed = FALSE)
  ## Strip any trailing "/" characters
  path <- sub(x = path, pattern = "/$", replacement = "", fixed = FALSE)
  ## Collapse any sequence of more than one "/" character into a single "/"
  path <- gsub(x = path, pattern = "//+", replacement = "/", fixed = FALSE)
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
      FUN.VALUE = logical(1L)
    )
  }

  return(is_present)
}

#' Precompute index positions grouped by chunk
#'
#' For each chunk touched by `index`, returns the positions (1-based) within
#' each dimension of `index` that fall inside that chunk, together with the
#' within-chunk indices needed to extract values from the chunk array.
#'
#' @param index A list of integer vectors, one per dimension, giving the
#'   requested array indices (1-based).
#' @param metadata List of array metadata as returned by `.read_array_metadata()`.
#'   Used to derive chunk name keys via `.create_chunk_names()`.
#'
#' @returns A named list keyed by chunk names (same format as
#'   `.create_chunk_names()`, e.g. `"c/0/1/0"` for Zarr V3).  Each element is
#'   a list with two components:
#'   * `positions`: a per-dimension list of integer vectors of positions into
#'     the corresponding `index` vector that map to that chunk.
#'   * `index_in_chunk`: a per-dimension list of 1-based integer vectors
#'     giving the within-chunk coordinates corresponding to `positions`.
#'
#' @importFrom utils relist
#'
#' @keywords internal
.chunk_positions_by_chunk <- function(index, metadata) {
  chunk_shape <- as.integer(unlist(
    metadata$chunk_grid$configuration$chunk_shape
  ))
  index0 <- relist(as.integer(unlist(index)) - 1L, index)

  per_dim <- (unlist(index0) %/% rep(chunk_shape, times = lengths(index0))) |>
    relist(index0) |>
    lapply(function(x) split(seq_along(x), x))
  chunk_keys <- do.call(expand.grid, lapply(per_dim, names))
  key_strings <- .create_chunk_names(as.matrix(chunk_keys), metadata)
  setNames(
    lapply(seq_len(nrow(chunk_keys)), function(i) {
      positions <- mapply(
        \(d, k) d[[k]],
        per_dim,
        chunk_keys[i, ],
        SIMPLIFY = FALSE
      )
      index0_in_chunk <- mapply(
        \(idx, pos, cs) idx[pos] %% cs,
        index0,
        positions,
        chunk_shape,
        SIMPLIFY = FALSE
      )
      index_in_chunk <- relist(unlist(index0_in_chunk) + 1L, index0_in_chunk)
      list(positions = positions, index_in_chunk = index_in_chunk)
    }),
    key_strings
  )
}
