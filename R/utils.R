#' @keywords internal
check_index <- function(index, metadata) {
  index_len <- length(index)
  ## check we have the correct number of dimensions
  if (index_len != length(metadata$shape)) {
    stop(
      "The number of dimensions provided to 'index' does not match the shape of the array"
    )
  }

  ## If any dimensions are NULL transform into the entirety of that dimension
  ## Otherwise check provided indices are valid
  failed <- rep_len(FALSE, index_len)
  for (i in seq_len(index_len)) {
    if (is.null(index[[i]])) {
      index[[i]] <- seq_len(metadata$shape[[i]])
    } else if (any(index[[i]] < 1L) || any(index[[i]] > metadata$shape[[i]])) {
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
    stop("Unsupported data type: ", typestr$name, call. = FALSE)
  }

  entry <- SUPPORTED_V3_TYPES[[typestr]]
  if (is.null(entry)) {
    stop("Unsupported data type: ", typestr, call. = FALSE)
  }
  return(list(
    base_type = entry$base_type,
    nbytes = entry$nbytes
  ))
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
#' @param chunk_dim Integer vector of length equal to the number of dimensions
#'   of the array, specifying the size of each chunk in each dimension.
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
#' @noRd
.chunk_positions_by_chunk <- function(
  index,
  metadata,
  chunk_dim = unlist(metadata$chunk_grid$configuration$chunk_shape)
) {
  index0 <- lapply(index, reindex, from = 1L, to = 0L)
  # FIXME:
  # - make this work for compat sequence that don't start at one
  # - fold the second step (index_in_chunk) here
  if (
    all(vapply(index, is.compact, logical(1L))) &&
      all(vapply(index, min, integer(1L)) == 1L)
  ) {
    per_dim <- mapply(
      \(i, cs) {
        split(
          i,
          rep(
            ((min(i) - 1L) %/% cs):((max(i) - 1L) %/% cs),
            each = cs,
            length.out = length(i)
          )
        )
      },
      index,
      chunk_dim,
      SIMPLIFY = FALSE
    )
  } else {
    index0 <- unlist(index0)
    per_dim <- (index0 %/% rep(chunk_dim, times = lengths(index))) |>
      relist(index) |>
      lapply(function(x) split(seq_along(x), x))
    index0 <- relist(index0, index)
  }
  chunk_keys <- do.call(expand.grid, lapply(per_dim, names))
  key_strings <- .create_chunk_names(as.matrix(chunk_keys), metadata)
  setNames(
    lapply(seq_along(key_strings), function(i) {
      positions <- mapply(
        \(d, k) d[[k]],
        per_dim,
        chunk_keys[i, ],
        SIMPLIFY = FALSE
      )
      index_in_chunk <- mapply(
        \(idx, pos, cs) reindex(idx[pos] %% cs, from = 0L, to = 1L),
        index0,
        positions,
        chunk_dim,
        SIMPLIFY = FALSE
      )
      list(positions = positions, index_in_chunk = index_in_chunk)
    }),
    key_strings
  )
}
