#' @export
#' @rdname ZarrArray-deprecated
#'
#' @title Deprecated DelayedArray backend functions
#'
#' @description
#' `r lifecycle::badge('superseded')`
#'
#' The DelayedArray backend has moved to a dedicated package: the ZarrArray package
#' (<https://github.com/Bioconductor/ZarrArray>).
#'
#' @param ... Passed to the new function in the ZarrArray package.
#'
#' @examples
#' zarr_path <- system.file(
#'   "extdata",
#'   "zarr_examples",
#'   "column-first",
#'   "int32.zarr",
#'   package = "Rarr"
#' )
#' ZarrArray(
#'   zarr_path
#' )
#'
ZarrArray <- function(...) {
  lifecycle::deprecate_warn(
    when = "1.12.0",
    what = "ZarrArray()",
    with = "ZarrArray::ZarrArray()",
    details = paste(
      "The functions related to the DelayedArray backend have moved",
      "to the dedicated ZarrArray package (https://github.com/Bioconductor/ZarrArray)."
    )
  )
  if (requireNamespace("ZarrArray", quietly = TRUE)) {
    return(ZarrArray::ZarrArray(...))
  } else {
    stop("ZarrArray() requires the ZarrArray package")
  }
}

#' @export
#' @rdname ZarrArray-deprecated
writeZarrArray <- function(...) {
  lifecycle::deprecate_warn(
    when = "1.12.0",
    what = "writeZarrArray()",
    with = "ZarrArray::writeZarrArray()",
    details = paste(
      "The functions related to the DelayedArray backend have moved",
      "to the dedicated ZarrArray package (https://github.com/Bioconductor/ZarrArray)."
    )
  )
  if (requireNamespace("ZarrArray", quietly = TRUE)) {
    return(ZarrArray::writeZarrArray(...))
  } else {
    stop("writeZarrArray() requires the ZarrArray package")
  }
}
