#' @importFrom jsonlite write_json
.write_zarray <- function(
  path,
  array_shape,
  chunk_shape,
  data_type,
  fill_value,
  compressor,
  dimension_separator = ".",
  order = "C"
) {
  order <- toupper(order)
  if (!order %in% c("C", "F")) {
    stop("The 'order' argument must be either 'C' or 'F'")
  }

  zarray <- list(
    shape = array_shape,
    chunks = chunk_shape,
    dtype = data_type,
    fill_value = fill_value,
    dimension_separator = dimension_separator,
    order = order,
    zarr_format = 2,
    filters = NULL,
    compressor = compressor
  )
  write_json(zarray, path, auto_unbox = TRUE, pretty = 4, null = "null")
}

#' Read the .zattrs file associated with a Zarr array or group
#'
#' @param zarr_path A character vector of length 1. This provides the
#'   path to a Zarr array or group.
#' @param new.zattrs a list inserted to .zattrs at the \code{path}.
#' @param overwrite if \code{TRUE} (the default), existing .zattrs elements will be overwritten by \code{new.zattrs}.
#'
#' @importFrom jsonlite write_json
#'
#' @importFrom utils modifyList
#' @export
#' @examples
#' z1 <- withr::local_tempdir(fileext = ".zarr")
#' write_zattrs(z1, list(date = "2025-01-01", author = "Jane Doe"))
#'
write_zattrs <- function(zarr_path, new.zattrs = list(), overwrite = TRUE) {
  zarr_path <- .normalize_array_path(zarr_path)
  zattrs_path <- paste0(zarr_path, ".zattrs")

  if (is.null(names(new.zattrs))) {
    stop("list elements should be named")
  }

  if ("" %in% names(new.zattrs)) {
    message("Ignoring unnamed list elements")
    new.zattrs <- new.zattrs[nzchar(names(new.zattrs))]
  }

  if (file.exists(zattrs_path)) {
    old.zattrs <- read_json(zattrs_path)
    new.zattrs <- if (overwrite) {
      modifyList(old.zattrs, new.zattrs)
    } else {
      modifyList(new.zattrs, old.zattrs)
    }
  }

  write_json(
    new.zattrs,
    zattrs_path,
    auto_unbox = TRUE,
    pretty = 4,
    null = "null"
  )
}
