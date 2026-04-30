# nolint next: cyclocomp_linter.
.check_datatype <- function(data_type, fill_value, nchar = NULL) {
  # TODO: Error if fill_value is incompatible with data_type in Zarr v3.
  # V3 spec says:
  # "The permitted values depend on the data type.
  # Fill values for core data types are listed in Permitted fill values."
  if (missing(data_type) && missing(fill_value)) {
    stop(
      "Data type cannot be determined if both 'data_type' and 'fill_value' arguments are missing."
    )
  }
  if (missing(data_type) && !missing(fill_value)) {
    ## if we only have a fill value, infer the data type from that
    data_type <- storage.mode(fill_value)
  }

  ## if data type was supplied directly, always use that
  supported_types <- c(
    "|i1",
    "<i2",
    "<i4",
    "<i8",
    "|u1",
    "<u2",
    "<u4",
    "<u8",
    "<f4",
    "<f8",
    "|S",
    "<U",
    "|b1",
    "|O"
  )
  r_base_types <- c("integer", "double", "character", "logical")

  if (data_type %notin% supported_types) {
    if (data_type %notin% r_base_types) {
      stop(
        "Currently only able to write integer, double, character and logical arrays"
      )
    }
    data_type <- switch(
      data_type,
      "integer" = "<i4",
      "double" = "<f8",
      "character" = "|S",
      "logical" = "|b1"
    )
  }

  ## set a default fill value if needed
  if (missing(fill_value)) {
    fill_value <- switch(
      data_type,
      "|i1" = 0L,
      "<i2" = 0L,
      "<i4" = 0L,
      "<i8" = 0L,
      "|u1" = 0L,
      "<u2" = 0L,
      "<u4" = 0L,
      "<u8" = 0L,
      "<f4" = 0,
      "<f8" = 0,
      "|S" = "",
      "<U" = "",
      "|O" = "",
      "|b1" = FALSE,
      NULL
    )
  }

  if (data_type %in% c("|S", "<U", ">U")) {
    if (is.null(nchar) || nchar < 1) {
      stop(
        "The 'nchar' argument must be provided when working with ",
        "character data types and be a positive integer"
      )
    }
    data_type <- paste0(data_type, as.integer(nchar))
  }

  double_types <- c("<f4", "<f8")
  # FIXME: the spec only defines this for floats but surely it makes
  # sense to also apply it to int and uint?
  if (is.na(fill_value) && data_type %in% double_types) {
    float_size <- switch(
      data_type,
      "<f4" = 4L,
      "<f8" = 8L
    )
    # "0xYYYYYYYY", specifying the byte representation of the floating point number as an unsigned integer.
    fill_value <- paste(
      c("0x", writeBin(fill_value, raw(), size = float_size, endian = "swap")),
      collapse = ""
    )
  }

  return(list(data_type = data_type, fill_value = fill_value))
}

#' Create an (empty) Zarr array
#'
#' @param zarr_array_path Character vector of length 1 giving the path to the
#'   new Zarr array.
#' @param dim Dimensions of the new array.  Should be a numeric vector with the
#'   same length as the number of dimensions.
#' @param dimension_names Optional character vector with the same length as
#'   `dim`.
#' @param chunk_dim Dimensions of the array chunks. Should be a numeric vector
#'   with the same length as the `dim` argument.
#' @param data_type Character vector giving the data type of the new array.
#'   Valid options are: "integer", "double", "character", "logical", which are
#'   based on standard R data types. You can also use the analogous Numpy
#'   formats: "|i1", "<i2", "<i4", "<f4", "<f8", "|S", "|b1".
#'   If this argument isn't provided the `fill_value` will be used to determine
#'   the datatype.
#' @param order Define the layout of the bytes within each chunk.  Valid options
#'   are 'column', 'row', 'F' & 'C'.  'column' or 'F' will specify
#'   "column-major" ordering, which is how R arrays are arranged in memory.
#'   'row' or 'C' will specify "row-major" order.
#' @param compressor What (if any) compression tool should be applied to the
#'   array chunks.  The default is to use `zstd` compression. Supplying `NULL`
#'   will disable chunk compression. See [compressors] for more details.
#' @param fill_value The default value for uninitialized portions of the array.
#'   Does not have to be provided, in which case the default for the specified
#'   data type will be used.
#' @param nchar For `datatype = "character"` this parameter gives the maximum
#'   length of the stored strings. It is an error not to specify this for a
#'   character array, but it is ignored for other data types.
#' @param dimension_separator The character used to to separate the dimensions
#'   in the names of the chunk files.  Valid options are limited to "." and "/".
#' @param zarr_version The version of the Zarr specification to use. Currently,
#'   either `2` or `3`. The default is `3`.
#'
#' @returns If successful returns (invisibly) `TRUE`.  However this function is
#'   primarily called for the size effect of initialising a Zarr array location
#'   and creating the `.zarray` metadata.
#'
#' @seealso [write_zarr_array()], [update_zarr_array()]
#'
#' @examples
#'
#' new_zarr_array <- file.path(tempdir(), "temp.zarr")
#' create_empty_zarr_array(new_zarr_array,
#'   dim = c(10, 20), chunk_dim = c(2, 5),
#'   data_type = "integer"
#' )
#'
#' @export
create_empty_zarr_array <- function(
  zarr_array_path,
  dim,
  chunk_dim,
  data_type,
  order = "F",
  compressor = use_zstd(),
  fill_value,
  nchar = NULL,
  dimension_separator = if (zarr_version == 2) "." else "/",
  dimension_names = NULL,
  zarr_version = 3
) {
  path <- .normalize_array_path(zarr_array_path)
  if (!dir.exists(path)) {
    dir.create(path)
  }

  dt <- .check_datatype(
    data_type = data_type,
    fill_value = fill_value,
    nchar = nchar
  )
  data_type <- dt$data_type
  fill_value <- dt$fill_value

  .check_chunk_shape(x_dim = dim, chunk_dim = chunk_dim)

  ## create the metadata file
  .write_zarr_metadata(
    array_path = path,
    array_shape = dim,
    dimension_names = dimension_names,
    chunk_shape = chunk_dim,
    data_type = data_type,
    order = order,
    fill_value = fill_value,
    compressor = compressor,
    dimension_separator = dimension_separator,
    zarr_version = zarr_version
  )

  return(invisible(TRUE))
}

#' Write an R array to Zarr
#'
#' @param x The R array that will be written to the Zarr array.
#' @param zarr_array_path Character vector of length 1 giving the path to the
#'   new Zarr array.
#' @param nchar For character arrays this parameter gives the maximum length of
#'   the stored strings. If this argument is not specified the array provided to
#'   `x` will be checked and the length of the longest string found will be used
#'   so no data are truncated. However this may be slow and providing a value to
#'   `nchar` can provide a modest performance improvement.
#' @inheritParams create_empty_zarr_array
#'
#' @note If `x` has `dimnames`, `names(dimnames(x))` will be stored as the
#' `dimension_names` field in the Zarr metadata.
#'
#' @returns The function is primarily called for the side effect of writing to
#'   disk. Returns (invisibly) `TRUE` if the array is successfully written.
#'
#' @examples
#'
#' new_zarr_array <- file.path(tempdir(), "integer.zarr")
#' x <- array(1:50, dim = c(10, 5))
#' write_zarr_array(
#'   x = x, zarr_array_path = new_zarr_array,
#'   chunk_dim = c(2, 5)
#' )
#'
#' @export
write_zarr_array <- function(
  x,
  zarr_array_path,
  chunk_dim,
  data_type = storage.mode(x),
  order = "F",
  compressor = use_zstd(),
  fill_value,
  nchar,
  dimension_separator = if (zarr_version == 2) "." else "/",
  zarr_version = 3
) {
  if (!is.array(x) && !is.atomic(x)) {
    stop(
      "`x` must be an atomic array. ",
      "You can maybe coerce your object with `as.array()`.",
      call. = FALSE
    )
  }
  path <- .normalize_array_path(zarr_array_path)

  if (storage.mode(x) == "character" && missing(nchar)) {
    # +1 to add NUL terminator
    # c(0, ) to deal with array full of NAs
    # base::nchar() to avoid collision with var name
    nchar <- max(c(0, base::nchar(x)), na.rm = TRUE) + 1
  }

  create_empty_zarr_array(
    zarr_array_path = path,
    dim = dim(x),
    dimension_names = names(dimnames(x)),
    chunk_dim = chunk_dim,
    data_type = data_type,
    order = order,
    fill_value = fill_value,
    compressor = compressor,
    nchar = nchar,
    dimension_separator = dimension_separator,
    zarr_version = zarr_version
  )
  ## read the metadata we just created
  metadata_file <- if (zarr_version == 2) ".zarray" else "zarr.json"
  metadata_v3 <- .read_array_metadata(path, metadata_file)

  metadata_v3$configured_encoders <- .configure_codecs(
    codecs = metadata_v3$codecs,
    operation = "encode"
  )

  chunk_indices <- .generate_chunk_indices(
    x_dim = dim(x),
    chunk_dim = chunk_dim
  )
  chunk_names <- .create_chunk_names(
    chunk_indices,
    metadata_v3
  )
  chunk_paths <- paste0(path, chunk_names)
  chunk_indices <- asplit(chunk_indices, 1, drop = TRUE)

  same_type_lower_bytesize <- metadata_v3$data_type %in%
    c("int8", "int16", "float32")
  lower_bytesize_type <- storage.mode(x) == "double" &&
    metadata_v3$data_type == "float32"

  can_overflow <- same_type_lower_bytesize || lower_bytesize_type
  if (can_overflow) {
    x <- .truncate_overflow(x, metadata_v3$datatype$nbytes)
  }

  if (metadata_v3$data_type == "bool" && anyNA(x)) {
    warning(
      "Zarr native 'bool' data type does not support NA values. ",
      "NA values will be converted to FALSE. ",
      "To preserve NA values, use 'uint8' datatype in `write_zarr_array()` and `as.logical()` after reading.",
      call. = FALSE
    )
  }

  ## iterate over each chunk
  ## TODO: maybe this can be done in parallel with bpmapply() ?
  res <- mapply(
    FUN = .write_chunk,
    chunk_paths,
    chunk_indices,
    MoreArgs = list(
      x = x,
      metadata = metadata_v3
    )
  )

  return(invisible(all(res)))
}

.generate_chunk_indices <- function(x_dim, chunk_dim) {
  n_chunks_in_dim <- (x_dim %/% chunk_dim) + as.logical(x_dim %% chunk_dim)
  expand.grid(lapply(n_chunks_in_dim, seq_len)) - 1
}

.write_chunk <- function(chunk_path, chunk_index, x, metadata) {
  chunk_dim <- unlist(metadata$chunk_grid$configuration$chunk_shape)

  idx_in_array <- list()
  for (j in seq_along(dim(x))) {
    idx_in_array[[j]] <- which(
      (seq_len(dim(x)[j]) - 1) %/% chunk_dim[j] == chunk_index[j]
    )
  }

  chunk_in_mem <- .extract_chunk(x, idx_in_array)

  # FIXME: can this check be faster?
  if (isTRUE(all(chunk_in_mem == metadata$fill_value))) {
    ## if the chunk only contains the fill value, we can skip writing it
    return(invisible(TRUE))
  }

  ## if a chunk overlaps the edge of the array, most implementations assume we
  ## still write the content to disk.  Seems wasteful, but we fail many tests
  if (any(dim(chunk_in_mem) != chunk_dim)) {
    ## create a new "complete" chunk
    temp_chunk <- array(dim = chunk_dim)

    ## insert our partial chunk into the new one
    idx_in_chunk <- lapply(dim(chunk_in_mem), seq_len)
    cmd <- .create_replace_call(
      "temp_chunk",
      "idx_in_chunk",
      length(idx_in_chunk),
      "chunk_in_mem"
    )
    eval(str2lang(cmd))
    ## update the output with the new full-sized chunk
    chunk_in_mem <- temp_chunk
  }

  .compress_and_write_chunk(
    input_chunk = chunk_in_mem,
    chunk_path = chunk_path,
    metadata
  )

  return(invisible(TRUE))
}

#' Update (a subset of) an existing Zarr array
#'
#' @param zarr_array_path Character vector of length 1 giving the path to the
#'   Zarr array that is to be modified.
#' @param x The R array (or object that can be coerced to an array) that will be
#'   written to the Zarr array.
#' @param index A list with the same length as the number of dimensions of the
#'   target array. This argument indicates which elements in the target array
#'   should be updated.
#'
#' @returns The function is primarily called for the side effect of writing to
#'   disk. Returns (invisibly) `TRUE` if the array is successfully updated.
#'
#' @examples
#'
#' ## first create a new, empty, Zarr array
#' new_zarry_array <- file.path(tempdir(), "new_array.zarr")
#' create_empty_zarr_array(
#'   zarr_array_path = new_zarry_array, dim = c(20, 10),
#'   chunk_dim = c(10, 5), data_type = "double"
#' )
#'
#' ## create a matrix smaller than our Zarr array
#' small_matrix <- matrix(runif(6), nrow = 3)
#'
#' ## insert the matrix into the first 3 rows, 2 columns of the Zarr array
#' update_zarr_array(new_zarry_array, x = small_matrix, index = list(1:3, 1:2))
#'
#' ## reading back a slightly larger subset,
#' ## we can see only the top left corner has been changed
#' read_zarr_array(new_zarry_array, index = list(1:5, 1:5))
#'
#' @export
update_zarr_array <- function(zarr_array_path, x, index) {
  stopifnot(is.list(index))

  zarr_array_path <- .normalize_array_path(zarr_array_path)

  metadata_files <- setNames(
    file.exists(paste0(zarr_array_path, c(".zarray", "zarr.json"))),
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
    names(metadata_files)[metadata_files]
  )

  index <- check_index(index, metadata = metadata)

  existing_storage <- switch(
    metadata$datatype$base_type,
    "uint" = "integer",
    "int" = "integer",
    "float" = "double",
    "bool" = "logical",
    "string" = "character",
    "unicode" = "character",
    NULL
  )
  if (!identical(storage.mode(x), existing_storage)) {
    stop("New data is not of the same type as the existing array.")
  }

  same_type_lower_bytesize <- metadata$data_type %in%
    c("int8", "int16", "float32")
  lower_bytesize_type <- storage.mode(x) == "double" &&
    metadata$data_type == "float32"

  can_overflow <- same_type_lower_bytesize || lower_bytesize_type
  if (can_overflow) {
    x <- .truncate_overflow(x, metadata$datatype$nbytes)
  }

  if (metadata$data_type == "bool" && anyNA(x)) {
    warning(
      "Zarr native 'bool' data type does not support NA values. ",
      "NA values will be converted to FALSE. ",
      "To preserve NA values, use 'uint8' datatype in `write_zarr_array()` and `as.logical()` after reading.",
      call. = FALSE
    )
  }

  metadata$configured_encoders <- .configure_codecs(
    codecs = metadata$codecs,
    operation = "encode"
  )
  metadata$configured_decoders <- .configure_codecs(
    codecs = metadata$codecs,
    operation = "decode"
  )

  zarr_dim <- unlist(metadata$shape)
  chunk_dim <- unlist(metadata$chunk_grid$configuration$chunk_shape)

  ## coerce x to the same shape as the zarr to be updated
  x <- array(x, dim = lengths(index))

  ## create all possible chunk names, then remove those that won't be touched
  chunk_indices <- .generate_chunk_indices(
    x_dim = zarr_dim,
    chunk_dim = chunk_dim
  )
  chunk_needed <- rep(FALSE, nrow(chunk_indices))

  ## precompute, for each chunk, the positions in `index` that belong to it
  chunk_positions <- .chunk_positions_by_chunk(index, as.list(chunk_dim))

  for (i in seq_len(nrow(chunk_indices))) {
    chunk_key <- paste(chunk_indices[i, ], collapse = ".")
    chunk_needed[i] <- !is.null(chunk_positions[[chunk_key]])
  }
  chunk_indices <- chunk_indices[chunk_needed, , drop = FALSE]
  chunk_names <- .create_chunk_names(
    chunk_indices,
    metadata
  )
  chunk_indices <- asplit(chunk_indices, 1, drop = TRUE)

  ## only update the chunks that need to be
  ## TODO: maybe this can be done in parallel is bpmapply() ?
  res <- mapply(
    FUN = .update_chunk,
    chunk_names,
    chunk_indices,
    MoreArgs = list(
      x = x,
      chunk_dim = chunk_dim,
      chunk_positions = chunk_positions,
      index = index,
      zarr_array_path = zarr_array_path,
      metadata = metadata
    )
  )

  return(invisible(all(res)))
}

.update_chunk <- function(
  chunk_name,
  chunk_index,
  x,
  zarr_array_path,
  chunk_dim,
  chunk_positions,
  index,
  # FIXME: once we fully switch to v3, we can remove this argument
  metadata
) {
  ## determine which elements of x are being used and where in this specific
  ## chunk they should be inserted
  chunk_path <- file.path(zarr_array_path, chunk_name)
  chunk_key <- paste(chunk_index, collapse = ".")
  idx_in_x <- chunk_positions[[chunk_key]]
  idx_in_zarr <- idx_in_chunk <- list()
  for (j in seq_along(chunk_dim)) {
    idx_in_zarr[[j]] <- index[[j]][idx_in_x[[j]]]
    idx_in_chunk[[j]] <- ((idx_in_zarr[[j]] - 1) %% chunk_dim[j]) + 1
  }

  if (.file_or_blob_exists(zarr_array_path, s3_client = NULL, chunk_name)) {
    chunk_in_mem <- read_chunk(
      chunk_path = chunk_path,
      metadata = metadata
    )
  } else {
    chunk_in_mem <- array(
      metadata$fill_value,
      dim = unlist(metadata$chunk_grid$configuration$chunk_shape)
    )
  }

  ## extract the new values from x and insert them into the chunk
  # nolint next: object_usage_linter.
  y <- .extract_chunk(x, idx_in_x)
  cmd <- .create_replace_call(
    "chunk_in_mem",
    "idx_in_chunk",
    length(idx_in_chunk),
    "y"
  )
  eval(str2lang(cmd))
  ## re-compress updated chunk and write back to disk
  .compress_and_write_chunk(
    input_chunk = chunk_in_mem,
    chunk_path = chunk_path,
    metadata
  )
}

#' Compress and write a single chunk
#'
#' @param input_chunk Array containing the chunk data to be compressed.  Will be
#'   converted to a raw vector before compression.
#' @param chunk_path Character string giving the path to the chunk that should
#'   be written.
#'
#' @returns Returns `TRUE` if writing is successful.  Mostly called for the
#'   side-effect of writing the compressed chunk to disk.
#'
#' @keywords internal
.compress_and_write_chunk <- function(
  input_chunk,
  chunk_path,
  metadata
) {
  ## check the chunk path exists, and create if not
  if (!dir.exists(dirname(chunk_path))) {
    dir.create(dirname(chunk_path), recursive = TRUE, showWarnings = FALSE)
  }

  # Array to array codecs
  for (codec in metadata$configured_encoders[["array_array"]]) {
    input_chunk <- do.call(codec, list(input_chunk))
  }
  # Array to bytes codecs
  for (codec in metadata$configured_encoders[["array_bytes"]]) {
    raw_chunk <- codec(as.vector(input_chunk), metadata$datatype)
  }

  # Bytes to bytes codecs
  codecs <- metadata$codecs
  compressor <- NULL
  compressor$id <- names(codecs)[match(
    TRUE,
    names(codecs) %in% c("zstd", "blosc", "gzip", "zlib", "bz2", "lzma", "lz4")
  )]
  compressor_config <- codecs[[compressor$id]]$configuration
  if (is.na(compressor$id)) {
    compressed_chunk <- raw_chunk
  } else if (compressor$id == "blosc") {
    compressed_chunk <- codec_blosc_encode(raw_chunk, compressor_config)
  } else if (compressor$id == "zlib") {
    compressed_chunk <- memCompress(from = raw_chunk, type = "gzip")
  } else if (compressor$id == "gzip") {
    con <- gzfile(
      chunk_path,
      open = "wb",
      compression = compressor_config$level
    )
    on.exit(close(con))
  } else if (compressor$id == "bz2") {
    con <- bzfile(
      chunk_path,
      open = "wb",
      compression = compressor_config$level
    )
    on.exit(close(con))
  } else if (compressor$id == "lzma") {
    con <- xzfile(
      chunk_path,
      open = "wb",
      compression = compressor_config$level
    )
    on.exit(close(con))
  } else if (compressor$id == "lz4") {
    compressed_chunk <- codec_lz4_encode(raw_chunk)
  } else if (compressor$id == "zstd") {
    compressed_chunk <- codec_zstd_encode(raw_chunk, compressor_config)
  }

  if (compressor$id %in% c("gzip", "bz2", "lzma")) {
    writeBin(raw_chunk, con = con, useBytes = TRUE)
  } else {
    writeBin(compressed_chunk, con = chunk_path)
  }

  return(invisible(TRUE))
}

.check_chunk_shape <- function(x_dim, chunk_dim) {
  if (length(x_dim) != length(chunk_dim)) {
    stop("The dimensions of the chunk must equal the dimensions of the array.")
  }

  for (i in seq_along(x_dim)) {
    # spec says:
    # "The chunk shape elements are non-zero when the corresponding dimensions
    # of the arrays have non-zero length."
    if ((x_dim[i] < chunk_dim[i]) || (chunk_dim[i] == 0 && x_dim[i] != 0)) {
      stop("Chunk dimensions outside the extent of the array")
    }
  }

  return(invisible(TRUE))
}
