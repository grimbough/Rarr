#' @export
#' @rdname ZarrArray-deprecated
#'
#' @title Deprecated DelayedArray backend functions
#'
#' @description
#' The DelayedArray backend has moved to a dedicated package: the ZarrArray package
#' (<https://github.com/Bioconductor/ZarrArray>).
#'
#' @param ... Ignored.
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
  .Deprecated(
    "ZarrArray",
    package = "ZarrArray",
    msg = paste(
      "The functions related to the DelayedArray backend have moved",
      "to the dedicated ZarrArray package (https://github.com/Bioconductor/ZarrArray)",
      "under the same name."
    )
  )
}

#' @export
#' @rdname ZarrArray-deprecated
writeZarrArray <- function(...) {
  .Deprecated(
    "writeZarrArray",
    package = "ZarrArray",
    msg = paste(
      "The functions related to the DelayedArray backend have moved",
      "to the dedicated ZarrArray package (https://github.com/Bioconductor/ZarrArray)",
      "under the same name."
    )
  )
}
