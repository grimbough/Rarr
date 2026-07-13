#' create_zarr_group
#'
#' Create a new group in an existing Zarr store.
#'
#' @param zarr_path Character vector of length 1 giving the path to the
#'   Zarr group.
#' @param group Character vector of length 1 giving the name of the zarr group
#' @inheritParams create_empty_zarr_array
#'
#' @examples
#'
#' # create new zarr
#' dir.create(td <- tempfile())
#' new_zarr_path <- file.path(td, "test.zarr")
#' create_zarr(zarr_path = new_zarr_path)
#' dir.exists(new_zarr_path)
#'
#' # create new group
#' create_zarr_group(new_zarr_path, group = "foo")
#' dir.exists(file.path(new_zarr_path, "foo"))
#'
#' @export
create_zarr_group <- function(
  zarr_path,
  group,
  version
) {
  zarr_path <- .normalize_array_path(zarr_path)

  # if zarr store does not exist, make one,
  # otherwise parse version
  if (dir.exists(zarr_path)) {
    metadata <- .read_group_metadata(zarr_path)
    zarr_format <- metadata$zarr_format
    if (missing(version)) {
      version <- 3L
      if (zarr_format != version) {
        version <- zarr_format
      }
    } else if (zarr_format != version) {
      warning(
        "Requested `version` is ",
        version,
        " but the Zarr store in ",
        zarr_path,
        " is written in version ",
        zarr_format,
        ". ",
        "Thus, version will be fixed to ",
        zarr_format,
        " instead!",
        call. = FALSE
      )
      version <- zarr_format
    }
  } else {
    dir.create(zarr_path, showWarnings = FALSE)
    .write_group_metadata(zarr_path, version)
  }

  # Split "a/b/c" into c("a", "b", "c")
  split_group <- strsplit(group, split = "/", fixed = TRUE)[[1L]]
  if (length(split_group) > 1L) {
    # Build cumulative paths: c("a", "a/b", "a/b/c")
    split_group <- vapply(
      seq_along(split_group),
      function(x) paste(split_group[seq_len(x)], collapse = "/"),
      FUN.VALUE = character(1L)
    )

    # Keep only the target and its immediate parent:
    # split_group[1] = "a/b/c" (target), split_group[2] = "a/b" (parent)
    split_group <- rev(tail(split_group, 2L))

    # Recursively ensure the parent group exists before creating the target
    if (!dir.exists(file.path(zarr_path, split_group[2L]))) {
      create_zarr_group(
        zarr_path = zarr_path,
        group = split_group[2L],
        version = version
      )
    }
  }

  target_zarr_path <- file.path(zarr_path, split_group[1L])
  dir.create(target_zarr_path, showWarnings = FALSE)
  .write_group_metadata(target_zarr_path, version)
}

#' create_zarr
#'
#' Create a new zarr store.
#'
#' @param zarr_path Character vector of length 1 giving the path to the new
#'   Zarr store.
#' @inheritParams create_empty_zarr_array
#'
#' @examples
#' dir.create(td <- tempfile())
#' new_zarr_path <- file.path(td, "test.zarr")
#' create_zarr(zarr_path = new_zarr_path)
#' dir.exists(new_zarr_path)
#'
#' @export
create_zarr <- function(zarr_path, version = 3L) {
  create_zarr_group(
    zarr_path = zarr_path,
    group = "/",
    version = version
  )
}

.read_group_metadata <- function(zarr_path, s3_client = NULL) {
  # FIXME: remove ... argument after https://github.com/Bioconductor/ZarrArray/pull/7
  # is merged
  metadata_file <- c(".zgroup", "zarr.json") |>
    .store_check_exist(zarr_path, files = _, s3_client = s3_client)

  if (metadata_file[".zgroup"] && metadata_file["zarr.json"]) {
    stop(
      "The path contains both `.zgroup` (Zarr V2 specification) and ",
      "`zarr.json` (Zarr V3 specification) metadata files.\n",
      "An group must conform to either the Zarr V2 or V3 specification.",
      call. = FALSE
    )
  }

  metadata_path <- paste0(zarr_path, names(metadata_file)[metadata_file])

  metadata <- .read_json_file(metadata_path, s3_client)

  return(metadata)
}

#' @importFrom jsonlite write_json
.write_group_metadata <- function(zarr_path, version = 2L) {
  zarr_path <- normalizePath(zarr_path)
  metadata <- list(zarr_format = version)
  switch(
    as.character(version),
    `2` = {
      metadata_file <- file.path(zarr_path, ".zgroup")
    },
    `3` = {
      metadata_file <- file.path(zarr_path, "zarr.json")
      metadata[["node_type"]] <- "group"
    },
    stop("Incorrect Zarr version specified. Must be '2L' or '3L'.")
  )
  write_json(metadata, path = metadata_file, auto_unbox = TRUE)
}
