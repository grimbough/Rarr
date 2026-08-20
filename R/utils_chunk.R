.create_chunk_names <- function(chunk_indices, metadata) {
  if (nrow(chunk_indices) == 0L) {
    # In the DelayedArray framework, we can have integer(0) indices
    # https://github.com/Huber-group-EMBL/Rarr/issues/112.
    return(character(0L))
  }
  if (metadata$zarr_format == 3L && identical(metadata$shape, 1L)) {
    # Special case for scalar in v3
    return("c")
  }

  dim_separator <- metadata$chunk_key_encoding$configuration$separator %||% "/"

  # Faster and ready for https://github.com/zarr-developers/zarr-extensions/pull/69
  chunk_name_template <- paste(
    rep("%s", ncol(chunk_indices)),
    collapse = dim_separator
  )
  if (metadata$zarr_format == 3L) {
    chunk_name_template <- paste("c", chunk_name_template, sep = dim_separator)
  }

  chunk_names <- do.call(sprintf, c(chunk_name_template, chunk_indices))

  return(chunk_names)
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
  if (!is.integer(chunk_dim)) {
    chunk_dim <- as.integer(chunk_dim)
  }

  if (
    all(vapply(index, is.compact, logical(1L))) &&
      all(vapply(index, min, integer(1L)) == 1L)
  ) {
    per_dim <- mapply(
      \(i, cs) {
        res <- .Call(
          "chop_vec",
          i,
          cs,
          PACKAGE = "Rarr"
        )
        setNames(res, 0L:(length(res) - 1L))
      },
      index,
      chunk_dim,
      SIMPLIFY = FALSE
    )
    # Faster than nested lapply()
    in_chunk <- rapply(per_dim, seq_along, how = "list")
  } else {
    flat0 <- unlist(lapply(index, reindex, from = 1L, to = 0L))
    cs <- rep(chunk_dim, times = lengths(index))
    id <- flat0 %/% cs
    # We compute the remainder "manually" to avoid expensive %% call,
    # when %/% did all the work already
    rem <- flat0 - id * cs
    per_dim <- relist(id, index) |>
      lapply(\(x) split(seq_along(x), x))
    in_chunk <- mapply(
      \(rem, pd) lapply(pd, \(pos) rem[pos] + 1L),
      relist(rem, index),
      per_dim,
      SIMPLIFY = FALSE
    )
  }

  chunk_keys <- do.call(expand.grid, lapply(per_dim, names))
  key_strings <- .create_chunk_names(chunk_keys, metadata)
  key_pos <- do.call(expand.grid, lapply(per_dim, seq_along))

  # Transpose the list to get the result per chunk, rather than per dimension.
  setNames(
    lapply(seq_len(nrow(key_pos)), \(i) {
      k <- key_pos[i, ]
      list(
        positions = mapply(\(d, kk) d[[kk]], per_dim, k, SIMPLIFY = FALSE),
        index_in_chunk = mapply(\(d, kk) d[[kk]], in_chunk, k, SIMPLIFY = FALSE)
      )
    }),
    key_strings
  )
}
