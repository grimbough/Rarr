### =========================================================================
### writeZarrArray()
### -------------------------------------------------------------------------
###

#' Write arrays to Zarr
#'
#' Write array data to a Zarr backend via \pkg{DelayedArray}'s \linkS4class{RealizationSink} machinery.
#'
#' @aliases
#' writeZarrArray
#' ZarrRealizationSink
#' ZarrRealizationSink-class
#' write_block,ZarrRealizationSink-method
#' type,ZarrRealizationSink-method
#' chunkdim,ZarrRealizationSink-method
#' coerce,ZarrRealizationSink,ZarrMatrix-method
#' coerce,ZarrRealizationSink,ZarrArray-method
#' coerce,ZarrRealizationSink,ZarrArraySeed-method
#' coerce,ZarrRealizationSink,DelayedArray-method
#' coerce,ANY,ZarrArray-method
#' coerce,ANY,ZarrRealizationSink-method
#'
#' @name ZarrRealizationSink
NULL


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### ZarrRealizationSink objects
###
### The ZarrRealizationSink class is a concrete RealizationSink subclass that
### implements an ZarrArray realization sink.
###

setClass(
  "ZarrRealizationSink",
  contains = "RealizationSink",
  representation(
    ## Slots that support the RealizationSink constructor contract.
    dim = "integer",
    type = "character",
    ## Other slots.
    zarr_array_path = "character", # Single string.
    chunk_dim = "integer" # An integer vector parallel to the 'dim' slot
  )
)

#' @export
setMethod("type", "ZarrRealizationSink", function(x) x@type)

#' @export
setMethod("chunkdim", "ZarrRealizationSink", function(x) x@chunk_dim)


ZarrRealizationSink <- function(
  zarr_array_path = NULL,
  dim,
  type = "double",
  chunkdim = NULL,
  nchar = NULL
) {
  if (is.null(zarr_array_path)) {
    stop("must provide a path")
  } else {
    zarr_array_path <- .normalize_array_path(zarr_array_path)
  }

  if (is.null(chunkdim)) {
    stop("must provide chunk dimensions")
  } else {
    chunkdim <- as.integer(chunkdim)
  }

  create_empty_zarr_array(
    zarr_array_path,
    dim = dim,
    chunk_dim = chunkdim,
    data_type = type,
    nchar = nchar
  )

  new(
    "ZarrRealizationSink",
    dim = dim,
    type = type,
    zarr_array_path = zarr_array_path,
    chunk_dim = chunkdim
  )
}

setMethod(
  "write_block",
  "ZarrRealizationSink",
  function(sink, viewport, block) {
    starts <- start(viewport) - 1L
    index <- lapply(width(viewport), seq_len)
    index <- mapply(FUN = "+", starts, index, SIMPLIFY = FALSE)

    update_zarr_array(sink@zarr_array_path, x = block, index = index)
    sink
  }
)

#' @export
writeZarrArray <- function(x, zarr_array_path, chunk_dim = NULL, nchar = NULL) {
  if (storage.mode(x) == "character" && is.null(nchar)) {
    nchar <- max(base::nchar(x))
  }

  sink <- ZarrRealizationSink(
    zarr_array_path = zarr_array_path,
    dim = dim(x),
    type = type(x),
    chunkdim = chunk_dim,
    nchar = nchar
  )
  sink <- BLOCK_write_to_sink(sink, x)
  as(sink, "ZarrArray")
}

setAs("ZarrRealizationSink", "ZarrArraySeed", function(from) {
  ZarrArraySeed(from@zarr_array_path)
})

setAs("ZarrRealizationSink", "ZarrArray", function(from) {
  DelayedArray(as(from, "ZarrArraySeed"))
})

setAs("ZarrRealizationSink", "DelayedArray", function(from) {
  DelayedArray(as(from, "ZarrArraySeed"))
})
