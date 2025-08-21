### -------------------------
### classes
### -------------------------
#' @import methods
#' @import BiocGenerics
#' @import DelayedArray

setClass(
  "ZarrArraySeed",
  contains = "Array",
  slots = c(
    zarr_array_path = "character",
    dim = "integer",
    chunk_dim = "integer"
  )
)

### ---------------------------
### extract_array()
### ---------------------------

.extract_array_from_ZarrArraySeed <- function(x, index) {
  res <- read_zarr_array(zarr_array_path = x@zarr_array_path, index = index)
  return(res)
}

setMethod("extract_array", "ZarrArraySeed", .extract_array_from_ZarrArraySeed)

### ---------------------------
### chunkdim() getter
### ---------------------------

setMethod("chunkdim", "ZarrArraySeed", function(x) x@chunk_dim)

### ---------------------------
### path() getter
### ---------------------------

setMethod("path", "ZarrArraySeed", function(object) object@zarr_array_path)

### ---------------------------
### ZarrArraySeed constructor
### ---------------------------
ZarrArraySeed <- function(zarr_array_path) {
  ## normalise path - can be file path or S3 url
  zarr_array_path <- .normalize_array_path(zarr_array_path)
  ## get the array dimensions from the metadata
  metadata <- zarr_overview(zarr_array_path, as_data_frame = TRUE)
  dim <- unlist(metadata$dim)
  chunk_dim <- unlist(metadata$chunk_dim)
  base_type <- unlist(metadata$data_type)

  new(
    "ZarrArraySeed",
    zarr_array_path = zarr_array_path,
    dim = dim,
    chunk_dim = chunk_dim
  )
}

# .validate_ZarrArraySeed <- function(x) {
#
#   ## 'dim' slot.
#   msg <- S4Arrays:::validate_dim_slot(x, "dim")
#   if (!isTRUE(msg))
#     return(msg)
#
#   ## 'chunkdim' slot.
#   x_chunkdim <- x@chunk_dim
#   if (!is.null(x_chunkdim)) {
#     msg <- S4Arrays:::validate_dim_slot(x, "chunk_dim")
#     if (!isTRUE(msg))
#       return(msg)
#   }
#
# }
#
# setValidity2("ZarrArraySeed", .validate_ZarrArraySeed)

### --------------------------------
### ZarrArray and ZarrMatrix objects
### --------------------------------

#' @aliases ZarrArray-class matrixClass,ZarrArray-method chunkdim,ZarrArraySeed-method
#' @rdname ZarrArray-classes
#'
#' @param zarr_array_path Path to a Zarr array. A character vector of length 1.
#'   This can either be a location on a local file system or the URI to an array
#'   in S3 storage.
#'
#' @return Object of class `ZarrArray`
#'
#' @exportClass ZarrArray
setClass(
  "ZarrArray",
  contains = "DelayedArray",
  representation(seed = "ZarrArraySeed")
)

#' @name ZarrMatrix
#' @aliases ZarrMatrix-class
#' @rdname ZarrArray-classes
#' @exportClass ZarrMatrix
setClass("ZarrMatrix", contains = c("ZarrArray", "DelayedMatrix"))

## for internal use only.
setMethod("matrixClass", "ZarrArray", function(x) "ZarrMatrix")

#' @name coerce
#' @export
#' @aliases coerce,ZarrArray,ZarrMatrix-method
#'     coerce,ZarrMatrix,ZarrArray-method coerce,ANY,ZarrMatrix-method
#' @rdname ZarrArray-classes
setAs("ZarrArray", "ZarrMatrix", function(from) new("ZarrMatrix", from))
setAs("ZarrMatrix", "ZarrArray", function(from) from)
setAs(
  "ANY",
  "ZarrMatrix",
  function(from) as(as(from, "ZarrArray"), "ZarrMatrix")
)

### --------------
### ZarrArray constructor
### --------------
setMethod(
  "DelayedArray",
  "ZarrArraySeed",
  function(seed) {
    new_DelayedArray(seed, Class = "ZarrArray")
  }
)

#' @title ZarrArray constructor
#' @description `ZarrayArray`: function to create a new object of class `ZarrArray`.
#'
#' @aliases ZarrArray-method
#' @rdname ZarrArray-classes
#' @export
ZarrArray <- function(zarr_array_path) {
  if (is(zarr_array_path, "ZarrArraySeed")) {
    seed <- zarr_array_path
  } else {
    seed <- ZarrArraySeed(zarr_array_path)
  }
  DelayedArray(seed)
}
