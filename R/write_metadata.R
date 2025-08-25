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
  zarray <- list()

  if (!toupper(order) %in% c("C", "F")) {
    stop("The 'order' argument must be either 'C' or 'F'")
  }

  zarray$shape <- array_shape
  zarray$chunks <- chunk_shape
  zarray$dtype <- data_type
  zarray$fill_value <- fill_value
  zarray$dimension_separator <- dimension_separator
  zarray$order <- toupper(order)
  zarray$zarr_format <- 2

  ## weird hack to insert a named NULL entry in the list
  zarray[length(zarray) + 1] <- list(NULL)
  names(zarray)[length(zarray)] <- "filters"

  if (is.null(compressor)) {
    zarray[length(zarray) + 1] <- list(NULL)
    names(zarray)[length(zarray)] <- "compressor"
  } else {
    zarray$compressor <- compressor
  }

  json <- .format_json(toJSON(
    zarray,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  ))
  write(x = json, file = path)
}

#' Read the .zattrs file associated with a Zarr array or group
#'
#' @param path A character vector of length 1. This provides the
#'   path to a Zarr array or group.
#' @param new.zattrs a list inserted to .zattrs at the \code{path}.
#' @param overwrite if \code{TRUE} (the default), existing .zattrs elements will be overwritten by \code{new.zattrs}.
#'
#' @importFrom jsonlite toJSON
#'
#' @importFrom utils modifyList
#' @export
write_zattrs <- function(path, new.zattrs = list(), overwrite = TRUE) {
  path <- .normalize_array_path(path)
  zattrs_path <- paste0(path, ".zattrs")

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

  json <- .format_json(toJSON(
    new.zattrs,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  ))
  write(x = json, file = zattrs_path)
}

.format_json <- function(json) {
  json <- gsub(x = json, pattern = "[", replacement = "[\n    ", fixed = TRUE)
  json <- gsub(x = json, pattern = "],", replacement = "\n  ],", fixed = TRUE)
  json <- gsub(x = json, pattern = ", ", replacement = ",\n    ", fixed = TRUE)
  return(json)
}
