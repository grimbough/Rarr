#' @importFrom jsonlite write_json
#' @importFrom grumpy parse_npy_datatype
.write_zarr_metadata <- function(
  array_path,
  array_shape,
  dimension_names = NULL,
  chunk_shape,
  data_type,
  fill_value,
  compressor,
  dimension_separator = ".",
  order = "C",
  zarr_version = 3L
) {
  if (!is.null(compressor$id) && compressor$id == "blosc") {
    nbytes <- parse_npy_datatype(data_type)$nbytes
    compressor$typesize <- compressor$typesize %||% nbytes
    if (is.na(compressor$typesize)) {
      # e.g., vlen-utf8
      compressor$typesize <- 1L
    }
  }

  metadata_v2 <- list(
    # the spec states these need to be json arrays, so we need to avoid auto_unboxing
    shape = as.list(array_shape),
    chunks = as.list(chunk_shape),
    dtype = data_type,
    fill_value = fill_value,
    dimension_separator = dimension_separator,
    order = order,
    zarr_format = 2L,
    filters = NULL,
    compressor = compressor
  )

  if (zarr_version == 2L) {
    if (!is.null(dimension_names)) {
      # The spec says we "SHOULDN'T" do this.
      metadata_v2$dimension_names <- dimension_names
    }
    write_json(
      metadata_v2,
      file.path(array_path, ".zarray"),
      auto_unbox = TRUE,
      pretty = TRUE,
      null = "null"
    )
    return(invisible(TRUE))
  }
  if (zarr_version == 3L) {
    metadata_v2$datatype <- parse_npy_datatype(data_type)
    metadata_v3 <- .convert_metadata_version(
      metadata_v2,
      version_from = 2L,
      version_to = 3L
    )
    metadata_v3$zarr_format <- 3L
    if (!is.null(dimension_names)) {
      metadata_v3$dimension_names <- dimension_names
    }
    # FIXME: get rid of these directly in the internal
    metadata_v3$codecs <- unname(metadata_v3$codecs)
    metadata_v3$datatype <- NULL
    write_json(
      metadata_v3,
      file.path(array_path, "zarr.json"),
      auto_unbox = TRUE,
      pretty = TRUE,
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
  metadata_v2 <- .store_check_exist(
    zarr_path,
    METADATA_V2_FILES
  )
  has_metadata_v2 <- any(metadata_v2)
  has_metadata_v3 <- .store_check_exist(zarr_path, METADATA_V3_FILES)
  if (has_metadata_v2 && has_metadata_v3) {
    stop(
      "Found ",
      toString(paste0("`", names(metadata_v2[metadata_v2]), "`")),
      " (Zarr V2 specification) and `zarr.json` (Zarr V3 specification). ",
      "Please resolve this conflict before writing attributes.",
      call. = FALSE
    )
  }
  stopifnot(
    "`zarr_version` must be 2 or 3" = zarr_version %in% c(2L, 3L),
    "list elements should be named" = !is.null(names(new.zattrs))
  )
  zarr_path <- .normalize_array_path(zarr_path)
  attrs_path <- if (zarr_version == 2L) {
    paste0(zarr_path, ".zattrs")
  } else {
    paste0(zarr_path, "zarr.json")
  }

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
    metadata <- new.zattrs
  } else if (zarr_version == 3L) {
    if (!has_metadata_v3) {
      stop(
        "Attributes can only be written to a Zarr array or group that already",
        " has a `zarr.json` file.\n",
        "Please create the Zarr array (with `write_zarr_array()` or ",
        "`create_empty_zarr_array()`) or group (with `write_zarr_group()`) ",
        "first BEFORE adding attributes.",
        call. = FALSE
      )
    }
    # FIXME: we really want a partial write to the json file
    metadata <- read_json(file.path(zarr_path, "zarr.json"))
    metadata$attributes <- new.zattrs
  }
  write_json(
    metadata,
    attrs_path,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )
  invisible(new.zattrs)
}

#' Consolidate Zarr metadata files into a single file
#'
#' This function reads all the metadata files in a Zarr store and consolidates them into a single file.
#' Thanks to this, a single request can be made to retrieve all the elements and their related metadata for a Zarr store,
#' which is especially beneficial for remote stores like S3.
#'
#' @param zarr_store_path A character vector of length 1. This provides the path to a Zarr store.
#' @inheritParams zarr_overview
#' @param action A character string specifying the action to take with the consolidated metadata.
#'   If `"write"` (the default), the consolidated metadata will be written back to the Zarr store.
#'   If `"return"`, the consolidated metadata will be returned as a list without writing it back to the store.
#'   The latter is particularly useful for non-writable stores.
#' @param overwrite A logical value (default `TRUE`) indicating whether to overwrite existing
#'   consolidated metadata when `action` is `"write"`. If `FALSE` and consolidated metadata
#'   already exists, an error will be raised.
#'
#' @returns If `action` is `"return"`, a list containing the consolidated metadata.
#'   Otherwise, the function is called for its side effect and `NULL` is returned invisibly.
#'
#' @importFrom jsonlite fromJSON read_json
#' @export
#'
#' @examples
#' # v2
#' zarr_v2 <- withr::local_tempfile(fileext = ".zarr")
#' dir.create(zarr_v2)
#' jsonlite::write_json(
#'   list("zarr_format" = 2L),
#'   file.path(zarr_v2, ".zgroup")
#' )
#' write_zarr_array(
#'   array(1:4, dim = c(2, 2)),
#'   file.path(zarr_v2, "array1"),
#'   chunk_dim = c(1, 2),
#'   zarr_version = 2L
#' )
#' write_zarr_array(
#'   array(c(3.14, 42.42, 12.96, 7.89), dim = c(2, 2)),
#'   file.path(zarr_v2, "array2"),
#'   chunk_dim = c(1, 2),
#'   zarr_version = 2L
#' )
#' write_zarr_attributes(
#'  file.path(zarr_v2, "array1"),
#'  list(description = "This is array 1")
#' )
#' zarr_consolidate_metadata(zarr_v2, action = "return")
#'
#' zarr_consolidate_metadata(zarr_v2, action = "write")
#' zarr_overview(zarr_v2)
#'
zarr_consolidate_metadata <- function(
  zarr_store_path,
  s3_client = NULL,
  action = c("write", "return"),
  overwrite = TRUE
) {
  action <- match.arg(action)

  zarr_store_path <- .normalize_array_path(zarr_store_path)
  s3_client <- s3_client %||% .create_s3_client(zarr_store_path)

  if (!is.null(s3_client) && action == "write") {
    warning(
      "Consolidating metadata on S3 is not currently supported. Returning consolidated metadata instead.",
      call. = FALSE
    )
    action <- "return"
  }

  if (action == "write" && !overwrite) {
    exists <- !is.null(
      suppressWarnings(
        # We don't want to see the warning about empty consolidated metadata.
        .read_consolidated_metadata(
          zarr_store_path,
          s3_client = s3_client
        )
      )
    )
    if (exists) {
      # TODO: return current consolidated metadata
      stop(
        "Consolidated metadata already exists. ",
        "Set `overwrite = TRUE` to overwrite it.",
        call. = FALSE
      )
    }
  }

  child_meta <- .store_list(zarr_store_path, recursive = TRUE, s3_client)

  # v2 or v3?
  metadata_v2_files <- endsWith(child_meta, ".zarray") |
    endsWith(child_meta, ".zgroup") |
    endsWith(child_meta, ".zattrs")
  metadata_v3_files <- endsWith(child_meta, "zarr.json")
  if (any(metadata_v2_files) && any(metadata_v3_files)) {
    stop(
      "Found both v2 and v3 metadata files. Please resolve this conflict before consolidating metadata.",
      call. = FALSE
    )
  }
  version <- if (any(metadata_v2_files)) 2L else 3L
  metadata_files <- child_meta[metadata_v2_files | metadata_v3_files]

  consolidated <- lapply(
    paste0(zarr_store_path, metadata_files),
    .read_json_file,
    s3_client = s3_client
  )

  if (version == 2L) {
    res <- list(
      zarr_consolidated_format = 1L,
      metadata = setNames(consolidated, metadata_files)
    )
  } else {
    attrs <- read_zarr_attributes(zarr_store_path, s3_client = s3_client)
    res <- list(
      zarr_format = 3L,
      node_type = "group",
      attributes = attrs,
      consolidated_metadata = list(
        kind = "inline",
        must_understand = FALSE,
        metadata = setNames(consolidated, dirname(metadata_files))
      )
    )
  }

  if (action == "return") {
    return(res)
  }

  if (version == 2L) {
    consolidated_path <- paste0(zarr_store_path, ".zmetadata")
  } else {
    consolidated_path <- paste0(zarr_store_path, "zarr.json")
  }

  write_json(
    res,
    consolidated_path,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )
}

#' Initialize a Zarr group
#'
#' @param zarr_path A character vector of length 1. This provides the
#'   path to a Zarr store.
#' @param group A character vector of length 1. This provides the name of the
#'   group to create. If `""`, the root group will be created.
#' @param zarr_version The version of the Zarr specification to use. If a
#'   metadata file already exists, the version will be inferred from the file.
#'   Otherwise, the default is `3`.
#'
#' @details Nested groups are created recursively. For example, if
#'   `group = "foo/bar"`, then the group `foo` will be created first,
#'   followed by the group `bar` inside of it.
#'
#' @export
#'
#' @examples
#' zarr_v2 <- withr::local_tempfile(fileext = ".zarr")
#' write_zarr_group(zarr_v2, "test/deep/nested/group", zarr_version = 2L)
#'
write_zarr_group <- function(
  zarr_path,
  group,
  zarr_version = if (has_metadata_v2) 2L else 3L
) {
  has_metadata_v2 <- any(.store_check_exist(
    zarr_path,
    METADATA_V2_FILES
  ))
  stopifnot(
    "`zarr_version` must be 2 or 3" = zarr_version %in% c(2L, 3L)
  )

  zarr_path <- .normalize_array_path(zarr_path)
  group_path <- paste0(zarr_path, group)
  parent_group_path <- dirname(group_path)

  metadata_file <- c(".zgroup", "zarr.json")[zarr_version - 1L]

  if (!.store_check_exist(parent_group_path, metadata_file)) {
    write_zarr_group(zarr_path, parent_group_path, zarr_version)
  }

  if (!dir.exists(group_path)) {
    dir.create(group_path)
  }

  meta <- list(zarr_format = zarr_version)
  group_metadata <- file.path(group_path, metadata_file)

  if (zarr_version == 3L) {
    if (
      file.exists(group_metadata) &&
        .read_json_file(group_metadata)$node_type != "group"
    ) {
      stop(
        "Cannot write group metadata to a non-group Zarr array",
        call. = FALSE
      )
    }
    meta$node_type <- "group"
  }

  write_json(
    meta,
    file.path(group_path, metadata_file),
    auto_unbox = TRUE,
    pretty = TRUE
  )
}
