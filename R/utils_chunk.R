.create_chunk_names <- function(chunk_indices, metadata) {
  # In the DelayedArray framework, we can have integer(0) indices
  # https://github.com/Huber-group-EMBL/Rarr/issues/112.
  if (nrow(chunk_indices) == 0L) {
    return(character(0L))
  }

  dim_separator <- metadata$chunk_key_encoding$configuration$separator %||% "/"

  # Faster and ready for https://github.com/zarr-developers/zarr-extensions/pull/69
  chunk_name_template <- paste(
    rep("%s", ncol(chunk_indices)),
    collapse = dim_separator
  )
  chunk_names <- do.call(sprintf, c(chunk_name_template, chunk_indices))

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
  if (!is.integer(chunk_dim)) {
    chunk_dim <- as.integer(chunk_dim)
  }
  # FIXME:
  # - make this work for compat sequence that don't start at one
  # - fold the second step (index_in_chunk) here
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
  } else {
    index0 <- unlist(index0)
    per_dim <- (index0 %/% rep(chunk_dim, times = lengths(index))) |>
      relist(index) |>
      lapply(function(x) split(seq_along(x), x))
    index0 <- relist(index0, index)
  }
  chunk_keys <- do.call(expand.grid, lapply(per_dim, names))
  key_strings <- .create_chunk_names(chunk_keys, metadata)
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
