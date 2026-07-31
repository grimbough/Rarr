# nolint next: cyclocomp_linter.
.check_datatype <- function(data_type, fill_value = NULL, nchar = NULL) {
  # TODO: Error if fill_value is incompatible with data_type in Zarr v3.
  # V3 spec says:
  # "The permitted values depend on the data type.
  # Fill values for core data types are listed in Permitted fill values."
  if (missing(data_type) && is.null(fill_value)) {
    stop(
      "Data type cannot be determined if both 'data_type' and 'fill_value' arguments are missing."
    )
  }
  if (missing(data_type) && !is.null(fill_value)) {
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
  fill_value <- fill_value %||%
    switch(
      data_type,
      "|i1" = 0L,
      "<i2" = 0L,
      "<i4" = 0L,
      "<i8" = 0L,
      "|u1" = 0L,
      "<u2" = 0L,
      "<u4" = 0L,
      "<u8" = 0L,
      "<f4" = 0.0,
      "<f8" = 0.0,
      "|S" = "",
      "<U" = "",
      "|O" = "",
      "|b1" = FALSE,
      NULL
    )

  if (data_type %in% c("|S", "<U", ">U")) {
    if (is.null(nchar) || nchar < 1L) {
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
#' @returns This function is primarily called for the side effect of
#'   initialising a Zarr array location and creating the `.zarray` or
#'   `zarr.json` metadata file.
#'   Returns (invisibly) the normalized path it wrote the metadata to.
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
  order = c("F", "C"),
  compressor = use_zstd(),
  fill_value = NULL,
  nchar = NULL,
  dimension_separator = if (zarr_version == 2L) "." else "/",
  dimension_names = NULL,
  zarr_version = 3L
) {
  if (!is.null(dimension_names) && length(dimension_names) != length(dim)) {
    stop(
      "`dimension_names` must have the same length as `dim`.",
      call. = FALSE
    )
  }
  order <- match.arg(order)

  dt <- .check_datatype(
    data_type = data_type,
    fill_value = fill_value,
    nchar = nchar
  )
  data_type <- dt$data_type
  fill_value <- dt$fill_value

  .check_chunk_shape(x_dim = dim, chunk_dim = chunk_dim)

  path <- .normalize_array_path(zarr_array_path)
  path_exists <- dir.exists(path)
  contents <- list.files(
    path,
    all.files = TRUE,
    no.. = TRUE,
    include.dirs = TRUE
  )
  path_is_empty <- length(contents) == 0L
  if (path_exists && !path_is_empty) {
    stop(
      "The specified `zarr_array_path` already exists and is non-empty. ",
      "Please provide a new `path` for the Zarr array.\n",
      "Attributes must be added AFTER the Zarr array is created.",
      call. = FALSE
    )
  }
  if (!path_exists) {
    dir.create(path)
  }

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

  return(invisible(path))
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
  order = c("F", "C"),
  compressor = use_zstd(),
  fill_value = NULL,
  nchar,
  dimension_separator = if (zarr_version == 2L) "." else "/",
  zarr_version = 3L
) {
  if (!is.array(x) && !is.atomic(x)) {
    stop(
      "`x` must be an atomic array. ",
      "You can maybe coerce your object with `as.array()`.",
      call. = FALSE
    )
  }

  if (storage.mode(x) == "character" && missing(nchar)) {
    # +1 to add NUL terminator
    # c(0, ) to deal with array full of NAs
    # base::nchar() to avoid collision with var name
    nchar <- max(c(0L, base::nchar(x)), na.rm = TRUE) + 1L
  }
  if (data_type == "integer") {
    data_type <- .guess_int_size(x, data_type)
  }

  path <- create_empty_zarr_array(
    zarr_array_path = zarr_array_path,
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
  # FIXME: it's not optimal to write and read again because we do multiple
  # steps to get it ready for writing, and then ready to use internally.
  # Related to https://github.com/Huber-group-EMBL/Rarr/issues/60.
  metadata <- .read_array_metadata(path)

  metadata$configured_encoders <- .configure_codecs(
    codecs = metadata$codecs,
    operation = "encode"
  )

  x <- .prepare_write_data(x, metadata)

  ## build index covering the entire array
  index <- lapply(dim(x), seq_len)

  ## precompute, for each chunk, the positions in `index` that belong to it
  chunk_positions <- .chunk_positions_by_chunk(index, metadata, chunk_dim)
  chunk_names <- names(chunk_positions)
  chunk_paths <- paste0(path, chunk_names)

  ## iterate over each chunk
  ## TODO: maybe this can be done in parallel with bpmapply() ?
  res <- mapply(
    FUN = .write_chunk,
    chunk_paths,
    chunk_names,
    MoreArgs = list(
      x = x,
      chunk_positions = chunk_positions,
      metadata = metadata
    )
  )

  return(invisible(all(res)))
}

.write_chunk <- function(chunk_path, chunk_name, x, chunk_positions, metadata) {
  chunk_dim <- unlist(metadata$chunk_grid$configuration$chunk_shape)

  chunk_info <- chunk_positions[[chunk_name]]
  idx_in_array <- chunk_info$positions
  idx_in_chunk <- chunk_info$index_in_chunk

  chunk_in_mem <- .extract_chunk(x, idx_in_array)

  # FIXME: can this check be faster?
  # isTRUE() because metadata$fill_value can be NA.
  if (isTRUE(all(chunk_in_mem == metadata$fill_value))) {
    ## if the chunk only contains the fill value, we can skip writing it
    return(invisible(TRUE))
  }

  # Spec says:
  # "Chunks at the border of an array always have the full chunk size,
  # even when the array only covers parts of it."
  if (any(dim(chunk_in_mem) != chunk_dim)) {
    ## create a new "complete" chunk filled with the fill value, then insert
    ## our partial chunk into it
    temp_chunk <- array(metadata$fill_value, dim = chunk_dim)
    rlang::inject(temp_chunk[!!!idx_in_chunk] <- chunk_in_mem) # nolint: implicit_assignment_linter.
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

  metadata <- .read_array_metadata(zarr_array_path)

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

  x <- .prepare_write_data(x, metadata)

  metadata$configured_encoders <- .configure_codecs(
    codecs = metadata$codecs,
    operation = "encode"
  )
  metadata$configured_decoders <- .configure_codecs(
    codecs = metadata$codecs,
    operation = "decode"
  )

  ## coerce x to the same shape as the zarr to be updated
  x <- array(x, dim = lengths(index))

  ## precompute, for each chunk, the positions in `index` that belong to it
  chunk_dim <- unlist(metadata$chunk_grid$configuration$chunk_shape)
  chunk_positions <- .chunk_positions_by_chunk(
    index,
    metadata,
    chunk_dim
  )
  chunk_names <- names(chunk_positions)

  ## only update the chunks that need to be
  ## TODO: maybe this can be done in parallel is bpmapply() ?
  res <- vapply(
    chunk_names,
    .update_chunk,
    x = x,
    chunk_positions = chunk_positions,
    zarr_array_path = zarr_array_path,
    chunk_dim = chunk_dim,
    metadata = metadata,
    FUN.VALUE = logical(1L)
  )

  return(invisible(all(res)))
}

.update_chunk <- function(
  chunk_name,
  x,
  zarr_array_path,
  chunk_positions,
  chunk_dim,
  metadata
) {
  ## determine which elements of x are being used and where in this specific
  ## chunk they should be inserted
  chunk_path <- file.path(zarr_array_path, chunk_name)
  chunk_info <- chunk_positions[[chunk_name]]
  idx_in_x <- chunk_info$positions
  idx_in_chunk <- chunk_info$index_in_chunk

  if (.store_check_exist(zarr_array_path, chunk_name, s3_client = NULL)) {
    raw_chunk <- .store_get_bytes(
      path = chunk_path,
      s3_client = NULL,
      s3_bucket = NULL
    )
    chunk_in_mem <- read_chunk(
      chunk_bytes = raw_chunk,
      chunk_dim = chunk_dim,
      decoders = metadata$configured_decoders,
      datatype = metadata$datatype,
      fill_value = metadata$fill_value
    )
  } else {
    chunk_in_mem <- array(
      metadata$fill_value,
      dim = chunk_dim
    )
  }

  ## extract the new values from x and insert them into the chunk
  rlang::inject(chunk_in_mem[!!!idx_in_chunk] <- x[!!!idx_in_x]) # nolint: implicit_assignment_linter.
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
  # Array to array codecs
  for (codec in metadata$configured_encoders[["array_array"]]) {
    input_chunk <- codec(input_chunk)
  }
  # Array to bytes codecs
  for (codec in metadata$configured_encoders[["array_bytes"]]) {
    raw_chunk <- codec(as.vector(input_chunk), metadata$datatype)
  }
  # Bytes to bytes codecs
  for (codec in metadata$configured_encoders[["bytes_bytes"]]) {
    raw_chunk <- codec(bytes = raw_chunk)
  }

  ## check the chunk path exists, and create if not
  if (!dir.exists(dirname(chunk_path))) {
    dir.create(dirname(chunk_path), recursive = TRUE, showWarnings = FALSE)
  }
  writeBin(raw_chunk, con = chunk_path)

  return(invisible(TRUE))
}

.check_chunk_shape <- function(x_dim, chunk_dim) {
  if (length(x_dim) != length(chunk_dim)) {
    stop("The dimensions of the chunk must equal the dimensions of the array.")
  }

  oversized_chunk <- any(chunk_dim > x_dim)
  if (oversized_chunk) {
    # One valid use case is nullable arrays in anndata.
    warning(
      "Chunk dimensions are larger than array dimensions. ",
      "This is allowed by the Zarr specification but may lead to inefficient storage and retrieval.\n",
      "In most cases, this is likely to be a mistake. Please check your `chunk_dim` argument.",
      call. = FALSE
    )
  }

  # https://github.com/zarr-developers/zarr-specs/issues/378
  # we should potentially also check that the chunk size is not 0,
  # but this is a conscious spec deviation we support
  return(invisible(!oversized_chunk))
}
