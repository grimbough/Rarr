#' Read a Zarr array
#'
#' @param zarr_array_path Path to a Zarr array. A character vector of length 1.
#'   This can either be a location on a local file system or the URI to an array
#'   in S3 storage.
#' @param index A list of the same length as the number of dimensions in the
#'   Zarr array.  Each entry in the list provides the indices in that dimension
#'   that should be read from the array.  Setting a list entry to `NULL` will
#'   read everything in the associated dimension.  If this argument is missing
#'   the entirety of the the Zarr array will be read.
#' @param s3_client Object created by [paws.storage::s3()]. Only required for a
#'   file on S3. Leave as `NULL` for a file on local storage.
#'
#' @returns An array with the same number of dimensions as the input array. The
#'   extent of each dimension will correspond to the length of the values
#'   provided to the `index` argument.
#'
#' @examples
#'
#' ## Using a local file provided with the package
#' ## This array has 3 dimensions
#' z1 <- system.file("extdata", "zarr_examples", "row-first", "int32.zarr", package = "Rarr")
#'
#' ## read the entire array
#' read_zarr_array(zarr_array_path = z1)
#'
#' ## extract values for first 10 rows, all columns, first slice
#' read_zarr_array(zarr_array_path = z1, index = list(1:10, NULL, 1))
#'
#' \donttest{
#' ## using a Zarr file hosted on Amazon S3
#' ## This array has a single dimension with length 2729077
#' z2 <- "https://noaa-nwm-retro-v2-zarr-pds.s3.amazonaws.com/feature_id/"
#'
#' ## read the entire array
#' read_zarr_array(zarr_array_path = z2)
#'
#' ## read alternating elements
#' read_zarr_array(zarr_array_path = z2, index = list(seq(1, 576, 2)))
#' }
#'
#' @export
read_zarr_array <- function(zarr_array_path, index, s3_client) {
  zarr_array_path <- .normalize_array_path(zarr_array_path)
  ## determine if this is a local or S3 array
  if (missing(s3_client)) {
    s3_client <- .create_s3_client(path = zarr_array_path)
  }

  metadata_files <- .file_or_blob_exists(
    zarr_array_path,
    s3_client,
    c(".zarray", "zarr.json")
  )

  if (metadata_files[".zarray"] && metadata_files["zarr.json"]) {
    stop(
      "The path contains both `.zarray` (Zarr V2 specification) and ",
      "`zarr.json` (Zarr V3 specification) metadata files.\n",
      "An array or group must conform to either the Zarr V2 or V3 ",
      "specification.",
      call. = FALSE
    )
  }
  if (!any(metadata_files)) {
    stop(
      "The path does not contain any metadata files. ",
      "It must contain one of: ",
      "  - `.zarray` (Zarr V2 specification)\n",
      "  - `zarr.json` (Zarr V3 specification)",
      call. = FALSE
    )
  }

  metadata <- .read_array_metadata(
    zarr_array_path,
    names(metadata_files)[metadata_files],
    s3_client = s3_client
  )

  if (metadata$zarr_format == 2) {
    metadata <- .convert_metadata_version(
      metadata,
      version_from = 2,
      version_to = 3
    )
  }
  metadata$configured_codecs <- .configure_codecs(
    metadata$codecs,
    operation = "decode"
  )

  ## if no index provided we will return everything
  if (missing(index)) {
    index <- vector(mode = "list", length = length(metadata$shape))
  }
  index <- check_index(index = index, metadata = metadata)

  required_chunks <- as.matrix(find_chunks_needed(metadata, index))

  res <- read_data(required_chunks, zarr_array_path, s3_client, index, metadata)

  return(res)
}

.extract_elements <- function(
  current_chunk_index,
  current_chunk_name,
  metadata,
  index,
  zarr_array_path,
  s3_client,
  chunk_idx
) {
  ## find elements to select from the chunk and what in the output we replace
  index_in_result <- index_in_chunk <- list()
  alt_chunk_dim <- unlist(metadata$chunk_grid$configuration$chunk_shape)

  # FIXME: deal with this by rewriting the chunk grid in metadata after we supported
  # non-regular chunk grid
  for (j in seq_along(current_chunk_index)) {
    index_in_result[[j]] <- which(chunk_idx[[j]] == current_chunk_index[j])
    ## are we requesting values outside the array due to overhanging chunks?
    outside_extent <- index_in_result[[j]] > metadata$shape[[j]]
    if (any(outside_extent)) {
      index_in_result[[j]] <- index_in_result[[j]][-outside_extent]
    }
    if (any(index_in_result[[j]] == metadata$shape[[j]])) {
      alt_chunk_dim[j] <- length(index_in_result[[j]])
    }

    index_in_chunk[[j]] <- ((index[[j]][index_in_result[[j]]] - 1) %%
      metadata$chunk_grid$configuration$chunk_shape[[j]]) +
      1
  }

  ## read this chunk
  chunk <- read_chunk(
    zarr_array_path,
    chunk_name = current_chunk_name,
    metadata = metadata,
    s3_client = s3_client,
    alt_chunk_dim = alt_chunk_dim
  )

  if (!is.null(chunk)) {
    ## extract the required elements from the chunk
    chunk <- R.utils::extract(
      chunk,
      indices = index_in_chunk,
      drop = FALSE
    )
  }

  return(list(chunk, index_in_result))
}


#' @importFrom R.utils extract
read_data <- function(
  required_chunks,
  zarr_array_path,
  s3_client,
  index,
  metadata
) {
  ## determine which chunk each of the requests indices belongs to
  # nolint next: undesirable_function_linter.
  chunk_idx <- mapply(
    \(x, y) {
      (x - 1) %/% y
    },
    index,
    metadata$chunk_grid$configuration$chunk_shape,
    SIMPLIFY = FALSE
  )

  chunk_names <- .create_chunk_names(
    required_chunks,
    metadata
  )

  warnings <- list()
  ## hopefully we can eventually do this in parallel
  chunk_selections <- withCallingHandlers(
    lapply(
      seq_along(chunk_names),
      function(i) {
        .extract_elements(
          current_chunk_index = required_chunks[i, ],
          current_chunk_name = chunk_names[i],
          metadata = metadata,
          index = index,
          zarr_array_path = zarr_array_path,
          s3_client = s3_client,
          chunk_idx = chunk_idx
        )
      }
    ),
    warning = function(w) {
      warnings <<- c(warnings, list(w)) # nolint: undesirable_operator_linter.
      invokeRestart("muffleWarning")
    }
  )
  for (w in unique(warnings)) {
    warning(w)
  }

  ## predefine our array to be populated from the read chunks
  output <- array(metadata$fill_value, dim = lengths(index))

  ## proceed in serial and update the output with each chunk selection in turn
  for (i in seq_along(chunk_selections)) {
    if (!is.null(chunk_selections[[i]][[1]])) {
      index_in_result <- chunk_selections[[i]][[2]]
      cmd <- .create_replace_call(
        x_name = "output",
        idx_name = "index_in_result",
        idx_length = length(index_in_result),
        y_name = "chunk_selections[[i]][[1]]"
      )
      eval(parse(text = cmd))
    }
  }
  return(output)
}

find_chunks_needed <- function(metadata, index) {
  index_chunks <- list()
  for (i in seq_along(index)) {
    index_chunks[[i]] <- unique(
      (index[[i]] - 1) %/% metadata$chunk_grid$configuration$chunk_shape[[i]]
    )
  }

  required_chunks <- expand.grid(index_chunks)
  return(required_chunks)
}

#' Determine the size of chunk in bytes after decompression
#'
#' @param datatype A list of details for the array datatype.  Expected to be
#' produced by [.parse_datatype()].
#' @param dimensions A list containing the dimensions of the chunk.  Expected
#' to be found in a list produced by [.read_array_metadata()].
#'
#' @returns An integer giving the size of the chunk in bytes
#'
#' @keywords internal
get_decompressed_chunk_size <- function(datatype, dimensions) {
  buffer_size <- prod(unlist(dimensions), datatype$nbytes)
  return(as.integer(buffer_size))
}

#' Read a single Zarr chunk
#'
#' @param zarr_array_path A character vector of length 1, giving the path to the
#'   Zarr array
#' @param chunk_name The name of the chunk to read.
#' @param metadata List produced by `.read_array_metadata()` holding the contents
#'   of the `.zarray` file. If missing this function will be called
#'   automatically, but it is probably preferable to pass the meta data rather
#'   than read it repeatedly for every chunk.
#' @param s3_client Object created by [paws.storage::s3()]. Only required for a
#'   file on S3. Leave as `NULL` for a file on local storage.
#' @param alt_chunk_dim The dimensions of the array that should be created from
#'   this chunk.  Normally this will be the same as the chunk shape in
#'   `metadata`, but when dealing with edge chunks, which may overlap the true
#'   extent of the array the returned array should be smaller than the chunk
#'   shape.
#' @param fill Logical of length 1.  If `TRUE`, missing chunks will be filled
#'    with the fill value from the array metadata.  If `FALSE` (the default),
#'    missing chunks will return `NULL`.
#'
#' @returns An array containing the decompressed chunk values.
#'
#' @keywords internal
read_chunk <- function(
  zarr_array_path,
  chunk_name,
  metadata,
  s3_client = NULL,
  alt_chunk_dim = NULL,
  fill = FALSE
) {
  chunk_file <- paste0(zarr_array_path, chunk_name)

  if (nzchar(Sys.getenv("RARR_DEBUG"))) {
    message(chunk_file)
  }

  if (is.null(s3_client)) {
    if (file.exists(chunk_file)) {
      size <- file.size(chunk_file)
      compressed_chunk <- readBin(con = chunk_file, what = "raw", n = size)
    } else {
      compressed_chunk <- NULL
    }
  } else {
    parsed_url <- parse_s3_path(chunk_file)

    if (.s3_object_exists(s3_client, parsed_url$bucket, parsed_url$object)) {
      compressed_chunk <- s3_client$get_object(
        Bucket = parsed_url$bucket,
        Key = parsed_url$object
      )$Body
    } else {
      compressed_chunk <- NULL
    }
  }

  # Missing chunks are filled with the fill value in the whole array.
  # We generally don't need to re-fill here and this saves resources.
  # A notable exception when we need to fill is in update_zarr_array().
  if (is.null(compressed_chunk)) {
    if (fill) {
      return(
        array(
          metadata$fill_value,
          dim = unlist(metadata$chunk_grid$configuration$chunk_shape)
        )
      )
    } else {
      return(NULL)
    }
  }

  # Bytes -> Bytes codecs
  for (codec in metadata$configured_codecs[["bytes_bytes"]]) {
    compressed_chunk <- codec(
      bytes = compressed_chunk
    )
  }
  decompressed_chunk <- compressed_chunk

  ## It doesn't seem clear if the on disk chunk will contain the overflow
  ## values or not, so we try both approaches.
  actual_chunk_size <- length(decompressed_chunk) / metadata$datatype$nbytes
  if (
    !is.null(metadata$codecs[["vlen_utf8"]]) ||
      (actual_chunk_size ==
        prod(unlist(metadata$chunk_grid$configuration$chunk_shape)))
  ) {
    chunk_dim <- unlist(metadata$chunk_grid$configuration$chunk_shape)
  } else {
    chunk_dim <- alt_chunk_dim
  }

  # Bytes -> Array codecs
  for (codec in metadata$configured_codecs[["array_bytes"]]) {
    converted_chunk <- do.call(
      codec,
      list(decompressed_chunk, chunk_dim, metadata)
    )
  }
  # Array -> Array codecs
  for (codec in metadata$configured_codecs[["array_array"]]) {
    converted_chunk <- do.call(codec, list(converted_chunk))
  }

  return(converted_chunk)
}
