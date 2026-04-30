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
read_zarr_array <- function(zarr_array_path, index, s3_client = NULL) {
  zarr_array_path <- .normalize_array_path(zarr_array_path)
  ## determine if this is a local or S3 array
  if (is.null(s3_client)) {
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
      "It must contain one of:\n",
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
  if (metadata$node_type == "group") {
    stop(
      "The provided path points to a Zarr group, but `read_zarr_array()` can ",
      "only read arrays. Please provide the path to an array within the group.",
      call. = FALSE
    )
  }

  metadata$configured_decoders <- .configure_codecs(
    metadata$codecs,
    operation = "decode"
  )

  ## if no index provided we will return everything
  if (missing(index)) {
    index <- vector(mode = "list", length = length(metadata$shape))
  }
  index <- check_index(index = index, metadata = metadata)

  res <- read_data(zarr_array_path, s3_client, index, metadata)

  if (!is.null(metadata$dimension_names)) {
    dimnames(res) <- setNames(
      vector("list", length = length(dim(res))),
      metadata$dimension_names
    )
  }

  return(res)
}


read_data <- function(
  zarr_array_path,
  s3_client,
  index,
  metadata
) {
  ## precompute, for each chunk, the positions in `index` that belong to it
  chunk_positions <- .chunk_positions_by_chunk(
    index,
    metadata$chunk_grid$configuration$chunk_shape,
    metadata
  )

  chunk_names <- names(chunk_positions)
  # In the DelayedArray framework, we can have integer(0) indices
  # https://github.com/Huber-group-EMBL/Rarr/issues/112
  chunk_paths <- paste0(zarr_array_path, chunk_names, recycle0 = TRUE)

  ## Vectorized check for chunk existence
  chunk_exists <- .file_or_blob_exists(zarr_array_path, s3_client, chunk_names)
  existing_idx <- which(chunk_exists)

  warnings <- list()
  ## hopefully we can eventually do this in parallel
  chunk_selections <- withCallingHandlers(
    lapply(
      # We skip missing chunks here since they will just be filled with the fill value
      # when initializing the consolidated array.
      existing_idx,
      function(i) {
        .extract_elements(
          chunk_name = chunk_names[i],
          current_chunk_path = chunk_paths[i],
          metadata = metadata,
          index = index,
          s3_client = s3_client,
          chunk_positions = chunk_positions
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
    index_in_result <- chunk_selections[[i]][[2]]
    cmd <- .create_replace_call(
      x_name = "output",
      idx_name = "index_in_result",
      idx_length = length(index_in_result),
      y_name = "chunk_selections[[i]][[1]]"
    )
    eval(str2lang(cmd))
    if (metadata$datatype$base_type == "structured") {
      # Assigning a list drops the dim attribute so we have to continuously add it again
      dim(output) <- lengths(index)
    }
  }
  return(output)
}

.extract_elements <- function(
  chunk_name,
  current_chunk_path,
  metadata,
  index,
  zarr_array_path,
  s3_client,
  chunk_positions
) {
  ## find elements to select from the chunk and what in the output we replace
  index_in_result <- chunk_positions[[chunk_name]]
  index_in_chunk <- list()
  alt_chunk_dim <- lengths(index_in_result)

  for (j in seq_along(index)) {
    index_in_chunk[[j]] <- ((index[[j]][index_in_result[[j]]] - 1) %%
      metadata$chunk_grid$configuration$chunk_shape[[j]]) +
      1
  }

  ## read this chunk
  chunk <- read_chunk(
    chunk_path = current_chunk_path,
    metadata = metadata,
    s3_client = s3_client,
    alt_chunk_dim = alt_chunk_dim
  )

  ## extract the required elements from the chunk
  # FIXME: optimization: skip this step if we are taking everything in the chunk
  chunk <- .extract_chunk(chunk, index_in_chunk)
  return(list(chunk, index_in_result))
}

#' Read a single Zarr chunk
#'
#' @param chunk_path A character vector of length 1, giving the path to the
#'   chunk to be read.
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
#'
#' @returns An array containing the decompressed chunk values.
#'
#' @keywords internal
read_chunk <- function(
  chunk_path,
  metadata,
  s3_client = NULL,
  alt_chunk_dim = NULL
) {
  # When we get here, we know the chunk exists, so we can read it without worrying about
  # handling missing.
  if (nzchar(Sys.getenv("RARR_DEBUG"))) {
    message(chunk_path)
  }

  if (is.null(s3_client)) {
    size <- file.size(chunk_path)
    raw_chunk <- readBin(con = chunk_path, what = "raw", n = size)
  } else {
    parsed_url <- parse_s3_path(chunk_path)
    raw_chunk <- s3_client$get_object(
      Bucket = parsed_url$bucket,
      Key = parsed_url$object
    )$Body
  }

  # Bytes -> Bytes codecs
  for (codec in metadata$configured_decoders[["bytes_bytes"]]) {
    raw_chunk <- codec(
      bytes = raw_chunk
    )
  }

  ## It doesn't seem clear if the on disk chunk will contain the overflow
  ## values or not, so we try both approaches.
  actual_chunk_size <- length(raw_chunk) / sum(metadata$datatype$nbytes)
  expected_chunk_size <- prod(unlist(
    metadata$chunk_grid$configuration$chunk_shape
  ))
  if (
    !is.null(metadata$codecs[["vlen_utf8"]]) ||
      actual_chunk_size == expected_chunk_size
  ) {
    chunk_dim <- unlist(metadata$chunk_grid$configuration$chunk_shape)
  } else {
    chunk_dim <- alt_chunk_dim
  }

  # Bytes -> Array codecs
  for (codec in metadata$configured_decoders[["array_bytes"]]) {
    converted_chunk <- codec(
      raw_chunk,
      chunk_dim,
      metadata$datatype
    )
  }
  # Array -> Array codecs
  for (codec in metadata$configured_decoders[["array_array"]]) {
    converted_chunk <- do.call(codec, list(converted_chunk))
  }

  return(converted_chunk)
}
