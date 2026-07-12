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
#' @importFrom lifecycle deprecate_warn
ZarrArray <- function(...) {
  deprecate_warn(
    when = "1.12.0",
    what = "ZarrArray()",
    with = "ZarrArray::ZarrArray()",
    details = paste(
      "The functions related to the DelayedArray backend have moved",
      "to the dedicated ZarrArray package (https://github.com/Bioconductor/ZarrArray)."
    )
  )
  if (!requireNamespace("ZarrArray", quietly = TRUE)) {
    stop("ZarrArray() requires the ZarrArray package")
  }
  ZarrArray::ZarrArray(...)
}

#' @export
#' @rdname ZarrArray-deprecated
writeZarrArray <- function(...) {
  deprecate_warn(
    when = "1.12.0",
    what = "writeZarrArray()",
    with = "ZarrArray::writeZarrArray()",
    details = paste(
      "The functions related to the DelayedArray backend have moved",
      "to the dedicated ZarrArray package (https://github.com/Bioconductor/ZarrArray)."
    )
  )
  if (!requireNamespace("ZarrArray", quietly = TRUE)) {
    stop("writeZarrArray() requires the ZarrArray package")
  }
  ZarrArray::writeZarrArray(...)
}
