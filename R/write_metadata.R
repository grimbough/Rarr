#' @importFrom jsonlite write_json
.write_zarr_metadata <- function(
  array_path,
  array_shape,
  chunk_shape,
  data_type,
  fill_value,
  compressor,
  dimension_separator = ".",
  order = "C",
  zarr_version = 3
) {
  metadata_v2 <- list(
    # the spec states these need to be json arrays, so we need to avoid auto_unboxing
    shape = as.list(array_shape),
    chunks = as.list(chunk_shape),
    dtype = data_type,
    fill_value = fill_value,
    dimension_separator = dimension_separator,
    order = order,
    zarr_format = 2,
    filters = NULL,
    compressor = compressor
  )

  if (zarr_version == 2) {
    write_json(
      metadata_v2,
      file.path(array_path, ".zarray"),
      auto_unbox = TRUE,
      pretty = 4,
      null = "null"
    )
    return(invisible(TRUE))
  }
  if (zarr_version == 3) {
    metadata_v2$datatype <- .parse_datatype(data_type)
    metadata_v3 <- .convert_metadata_version(
      metadata_v2,
      version_from = 2,
      version_to = 3
    )
    metadata_v3$zarr_format <- 3L
    # FIXME: get rid of these directly in the internal
    metadata_v3$codecs <- unname(metadata_v3$codecs)
    metadata_v3$datatype <- NULL
    write_json(
      metadata_v3,
      file.path(array_path, "zarr.json"),
      auto_unbox = TRUE,
      pretty = 4,
      null = "null"
    )
  }
}

#' Read the .zattrs file associated with a Zarr array or group
#'
#' @param zarr_path A character vector of length 1. This provides the
#'   path to a Zarr array or group.
#' @param new.zattrs a list inserted to .zattrs at the `path`.
#' @param overwrite if `TRUE` (the default), existing .zattrs elements will be overwritten by `new.zattrs`.
#' @param zarr_version The version of the Zarr specification to use. If a
#'   metadata file already exists, the version will be inferred from the file.
#'   Otherwise, the default is `3`.
#'
#' @importFrom jsonlite write_json
#'
#' @returns Invisibly, the updated attributes as a named list.
#' This is equivalent to (but faster than) using `read_zarr_attributes()` after writing.
#' If no attributes were present before, this is identical to `new.zattrs`.
#'
#' @importFrom utils modifyList
#' @export
#' @examples
#' z1 <- withr::local_tempdir(fileext = ".zarr")
#' write_zarr_attributes(z1, list(date = "2025-01-01", author = "Jane Doe"))
#'
write_zarr_attributes <- function(
  zarr_path,
  new.zattrs = list(),
  overwrite = TRUE,
  zarr_version = if (has_metadata_v2) 2L else 3L
) {
  has_metadata_v2 <- any(file.exists(file.path(
    zarr_path,
    c(".zarray", ".zgroup", ".zattrs")
  )))
  stopifnot(
    "`zarr_version` must be 2 or 3" = zarr_version %in% c(2L, 3L),
    "list elements should be named" = !is.null(names(new.zattrs))
  )
  zarr_path <- .normalize_array_path(zarr_path)

  if ("" %in% names(new.zattrs)) {
    message("Ignoring unnamed list elements")
    new.zattrs <- new.zattrs[nzchar(names(new.zattrs))]
  }

  old.zattrs <- read_zarr_attributes(zarr_path)

  new.zattrs <- if (overwrite) {
    modifyList(old.zattrs, new.zattrs)
  } else {
    modifyList(new.zattrs, old.zattrs)
  }

  if (zarr_version == 2L) {
    write_json(
      new.zattrs,
      file.path(zarr_path, ".zattrs"),
      auto_unbox = TRUE,
      pretty = 4,
      null = "null"
    )
  } else if (zarr_version == 3L) {
    # FIXME: we really want a partial write to the json file
    if (file.exists(file.path(zarr_path, "zarr.json"))) {
      metadata <- read_json(file.path(zarr_path, "zarr.json"))
    } else {
      metadata <- list(
        "zarr_format" = 3L
      )
    }
    metadata$attributes <- new.zattrs
    write_json(
      metadata,
      file.path(zarr_path, "zarr.json"),
      auto_unbox = TRUE,
      pretty = 4,
      null = "null"
    )
  }

  invisible(new.zattrs)
}
