#' Print a summary of a Zarr array
#'
#' When reading a Zarr array using [read_zarr_array()] it is necessary to know
#' it's shape and size. `zarr_overview()` can be used to get a quick overview of
#' the array shape and contents, based on the `.zarray` (Zarr v2) or `zarr.json`
#' (Zarr v3) metadata file each array contains.
#'
#' The function currently prints the following information to the R console:
#'  - array path
#'  - array shape and size
#'  - chunk and size
#'  - the number of chunks
#'  - the datatype of the array
#'  - codec used for data compression (if any)
#'
#' If given the path to a group of arrays the function will attempt to print the
#' details of all sub-arrays in the group.
#'
#' @param zarr_array_path A character vector of length 1.  This provides the
#'   path to a Zarr array or group of arrays. This can either be on a local file
#'   system or on S3 storage.
#' @param s3_client A list representing an S3 client.  This should be produced
#' by [paws.storage::s3()].
#' @param as_data_frame Logical determining whether the Zarr array details
#'   should be printed to screen (`FALSE`) or returned as a `data.frame`
#'   (`TRUE`) so they can be used computationally.
#'
#' @return If `as_data_frame = FALSE` the function invisible returns `TRUE` if
#'   successful.  However it is primarily called for the side effect of printing
#'   details of the Zarr array(s) to the screen.  If `as_data_frame = TRUE` then
#'   a `data.frame` containing details of the array is returned.
#'
#' @examples
#'
#' ## Using a local file provided with the package
#' z1 <- system.file("extdata", "zarr_examples", "row-first",
#'   "int32.zarr",
#'   package = "Rarr"
#' )
#'
#' ## read the entire array
#' zarr_overview(zarr_array_path = z1)
#'
#' ## using a file on S3 storage
#' \donttest{
#' z2 <- "https://noaa-nwm-retro-v2-zarr-pds.s3.amazonaws.com/feature_id/"
#' zarr_overview(z2)
#' }
#' @export
zarr_overview <- function(zarr_array_path, s3_client, as_data_frame = FALSE) {
  zarr_array_path <- .normalize_array_path(zarr_array_path)

  if (missing(s3_client)) {
    s3_client <- .create_s3_client(path = zarr_array_path)
  }

  metadata_files <- .file_or_blob_exists(
    zarr_array_path,
    s3_client,
    c(".zmetadata", ".zarray", "zarr.json")
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
      "  - `.zmetadata` (consolidated metadata)\n",
      "  - `.zarray` (Zarr V2 specification)\n",
      "  - `zarr.json` (Zarr V3 specification)",
      call. = FALSE
    )
  }

  # FIXME: let's not even try to read it if we know it doesn't exist
  dot_zmeta <- .read_zmetadata(
    zarr_path = zarr_array_path,
    s3_client = s3_client
  )
  if (!is.null(dot_zmeta)) {
    arrays <- grep(
      names(dot_zmeta$metadata),
      pattern = "/(\\.zarray|zarr\\.json)$",
      value = TRUE
    )

    tmp <- lapply(
      arrays,
      FUN = .rbind_array_metadata,
      metadata = dot_zmeta$metadata,
      zarr_array_path = zarr_array_path
    )
    res <- do.call(rbind.data.frame, tmp)
    if (as_data_frame) {
      return(res)
    }
    cat("Type: Group of Arrays\n")
    cat(
      "Path: ",
      normalizePath(zarr_array_path, mustWork = FALSE),
      "\n",
      sep = ""
    )
    cat("Arrays:\n---\n")
    .print_array_metadata(res, indent = "  ")
    invisible(TRUE)
  } else {
    metadata_files <- metadata_files[-1]
    array_metadata <- .read_array_metadata(
      zarr_path = zarr_array_path,
      metadata_file = names(metadata_files)[metadata_files],
      s3_client = s3_client
    )
    res <- .rbind_array_metadata(
      array_name = basename(zarr_array_path),
      metadata = array_metadata,
      zarr_array_path = dirname(zarr_array_path)
    )
    if (as_data_frame) {
      return(res)
    }
    cat("Type: Array\n")
    .print_array_metadata(res)
    invisible(TRUE)
  }
}

.rbind_array_metadata <- function(array_name, metadata, zarr_array_path) {
  if (array_name %in% names(metadata)) {
    array_metadata <- metadata[[array_name]]
    array_name <- dirname(array_name)
  } else {
    array_metadata <- metadata
  }

  # FIXME: once the data reading/writing code has been updated to use v3, this
  # should move to .read_array_metadata()
  if (array_metadata$zarr_format == 2) {
    array_metadata$datatype <- .parse_datatype(array_metadata$dtype)
    array_metadata <- .convert_metadata_version(
      array_metadata,
      version_from = 2,
      version_to = 3
    )
  }

  chunk_shape <- unlist(array_metadata$chunk_grid$configuration$chunk_shape)
  data_shape <- unlist(array_metadata$shape)
  nchunks <- ceiling(
    data_shape / chunk_shape
  )

  codecs <- array_metadata$codecs
  compressor <- names(codecs)[match(
    TRUE,
    names(codecs) %in% c("zstd", "blosc", "gzip")
  )]
  endianness <- codecs[["bytes"]][["configuration"]][["endian"]]

  res <- data.frame(
    path = paste0(.normalize_array_path(zarr_array_path), array_name),
    data_type = array_metadata$data_type,
    # We already introduce default values in .convert_metadata_version() but
    # these do not exist when reading v3 metadata directly.
    endianness = endianness %||% NA_character_,
    compressor = compressor %||% NA_character_
  )
  res$dim <- list(data_shape)
  res$chunk_dim <- list(chunk_shape)
  res$nchunks <- list(nchunks)
  return(res)
}

.print_array_metadata <- function(array_metadata_df, indent = "") {
  fields <- c(
    "Path",
    "Shape",
    "Chunk Shape",
    "No. of Chunks",
    "Data Type",
    "Endianness",
    "Compressor"
  )
  fields <- paste0(indent, fields, ": %s")
  formatted <- sprintf(
    paste(fields, collapse = "\n"),
    array_metadata_df$path,
    vapply(
      array_metadata_df$dim,
      function(x) {
        paste(unlist(x), collapse = " x ")
      },
      character(1)
    ),
    vapply(
      array_metadata_df$chunk_dim,
      function(x) {
        paste(unlist(x), collapse = " x ")
      },
      character(1)
    ),
    vapply(
      array_metadata_df$nchunks,
      function(x) {
        chunks <- unlist(x)
        paste0(prod(chunks), " (", paste(chunks, collapse = " x "), ")")
      },
      character(1)
    ),
    array_metadata_df$data_type,
    array_metadata_df$endianness,
    ifelse(
      is.na(array_metadata_df$compressor),
      "None",
      array_metadata_df$compressor
    )
  )
  cat(formatted, sep = "\n---\n")
}

#' Read the .zarray metadata file associated with a Zarr array
#'
#' @param zarr_path A character vector of length 1.  This provides the
#'   path to a Zarr array or group of arrays. This can either be on a local file
#'   system or on S3 storage.
#' @param metadata_file One of `".zarray"` (Zarr v2) or `"zarr.json"` (Zarr v3)
#'   specifying which metadata file to read.
#' @param s3_client A list representing an S3 client.  This should be produced
#' by [paws.storage::s3()].
#'
#' @returns A list containing the array metadata
#'
#' @importFrom jsonlite read_json fromJSON
#'
#' @keywords internal
.read_array_metadata <- function(zarr_path, metadata_file, s3_client = NULL) {
  zarr_path <- .normalize_array_path(zarr_path)
  metadata_path <- paste0(zarr_path, metadata_file)

  if (!is.null(s3_client)) {
    parsed_url <- parse_s3_path(metadata_path)

    s3_object_exists <- .s3_object_exists(
      s3_client,
      parsed_url$bucket,
      parsed_url$object
    )

    # We already checked this in zarr_overview(), but in the case of a terribly
    # broken Zarr store, a non-existent .zarray file could be listed in the
    # .zmetadata file.
    if (!s3_object_exists) {
      stop(
        sprintf(
          "The requested `%s` metadata file (%s) does not exist.",
          metadata_file,
          "possibly listed in `.zmetadata`"
        ),
        call. = FALSE
      )
    }

    s3_object <- s3_client$get_object(
      Bucket = parsed_url$bucket,
      Key = parsed_url$object
    )

    metadata <- fromJSON(rawToChar(s3_object$Body))
  } else {
    zarray_exists <- file.exists(metadata_path)

    # We already checked this in zarr_overview(), but in the case of a terribly
    # broken Zarr store, a non-existent .zarray file could be listed in the
    # .zmetadata file.
    if (!zarray_exists) {
      stop(
        sprintf(
          "The requested `%s` metadata file (%s) does not exist.",
          metadata_file,
          "possibly listed in `.zmetadata`"
        ),
        call. = FALSE
      )
    }

    metadata <- read_json(metadata_path)
  }

  if (metadata$zarr_format == 2) {
    ## if we do this here, we save many repeated calls to .parse_datatype
    ## the parsed version is used each time a chunk is read
    metadata$datatype <- .parse_datatype(metadata$dtype)
    metadata <- .update_fill_value(metadata, metadata$datatype)
  } else if (metadata$zarr_format == 3) {
    metadata$datatype <- .parse_datatype_v3(metadata$data_type)
    # We shouldn't have any case where x$name is NULL since the v3 spec states
    # 'name' MUST be a plain string.
    names(metadata$codecs) <- vapply(
      metadata$codecs,
      function(x) gsub("-", "_", x$name, fixed = TRUE),
      character(1)
    )
    if (length(metadata$shape) == 0) {
      # Empty tuple in shape means we are dealing with a scalar.
      metadata$shape <- 1
      metadata$chunk_grid <- list(
        name = "regular",
        configuration = list(chunk_shape = 1)
      )
    }
    # This needs to happen after we address the scalar edge case
    if (is.null(metadata$codecs[["transpose"]])) {
      # We need to make sure this is always present because we do the
      # reverse of what this codec is telling us (since R uses F-order).
      # So even when using the implicit default, we need to add it here.
      metadata$codecs[["transpose"]] <- list(
        name = "transpose",
        configuration = list(order = seq_along(metadata$shape) - 1)
      )
    }
  } else {
    stop(
      "Unsupported Zarr format version: ",
      metadata$zarr_format,
      call. = FALSE
    )
  }

  return(metadata)
}

#' Convert special fill values from strings to numbers
#'
#' Special case fill values (NaN, Inf, -Inf) are encoded as strings in the Zarr
#' metadata.  R will create arrays of type character if these are defined and
#' the chunk isn't present on disk. This function updates the fill value to be
#' R's representation of these special values, so numeric arrays are created. A
#' "null" fill value implies no missing values. We set this to NA as you can't
#' create an array of type NULL in R. It should have no impact if there are
#' really no missing values.
#'
#' @param metadata A list containing the array metadata.  This should normally
#'   be generated by running `read_json()` on the `.zarray` file.
#' @param datatype A list of details for the array datatype.  Expected to be
#' produced by [.parse_datatype()].
#'
#' @returns Returns a list with the same structure as the input.  The returned
#'   list will be identical to the input, unless the `fill_value` entry was on
#'   of: NULL, "NaN", "Infinity" or "-Infinity".
#'
#' @keywords internal
.update_fill_value <- function(metadata, datatype) {
  val <- metadata$fill_value
  ## a null fill value implies no missing values.
  ## We set to NA as you can't create an array of NULL in R
  if (is.null(val)) {
    metadata$fill_value <- NA
  } else if (val %in% c("NaN", "Infinity", "-Infinity")) {
    if (datatype$base_type != "string") {
      metadata$fill_value <- switch(
        val,
        "NaN" = NaN,
        "Infinity" = Inf,
        "-Infinity" = -Inf
      )
    }
  } else if (is.numeric(val)) {
    metadata$fill_value <- switch(
      datatype$base_type,
      "float" = as.double(val),
      "int" = as.integer(val),
      "uint" = as.integer(val),
      "complex" = as.complex(val),
      val
    )
  }
  return(metadata)
}

#' Read consolidated metadata file
#'
#' @details
#' This is stored in the `.zmetadata` file at the root of a Zarr store.
#' Note that it is not documented in the official Zarr specification, because
#' it is not (yet?) part of the standard.
#'
#' It is implemented in zarr-python and discussed under the
#' "consolidated metadata" phrase.
#'
#' In particular, it lists the location of all the metadata files for arrays in
#' the current group, so it is not necessary to crawl to discover them.
#'
#' @references
#' <https://zarr.readthedocs.io/en/latest/user-guide/consolidated_metadata.html>
#'
#'
#' @inheritParams .read_array_metadata
#'
#' @importFrom jsonlite read_json fromJSON
#' @keywords internal
.read_zmetadata <- function(zarr_path, s3_client) {
  zarr_path <- .normalize_array_path(zarr_path)
  zmeta_path <- paste0(zarr_path, ".zmetadata")
  zmeta <- NULL

  if (!is.null(s3_client)) {
    parsed_url <- parse_s3_path(zmeta_path)
    if (.s3_object_exists(s3_client, parsed_url$bucket, parsed_url$object)) {
      s3_object <- s3_client$get_object(
        Bucket = parsed_url$bucket,
        Key = parsed_url$object
      )

      zmeta <- fromJSON(rawToChar(s3_object$Body))
    }
  } else if (file.exists(zmeta_path)) {
    zmeta <- read_json(zmeta_path)
  }

  return(zmeta)
}

#' Read the attributes associated with a Zarr array or group
#'
#' @inheritParams .read_array_metadata
#'
#' @returns A list containing the attributes. If the file containing attributes
#' (`.zattrs` for Zarr v2 or `zarr.json` for Zarr v3) exists but no attributes
#' are provided, an empty list is returned.
#'
#' @importFrom jsonlite read_json fromJSON
#'
#' @export
read_zarr_attributes <- function(zarr_path, s3_client = NULL) {
  zarr_path <- .normalize_array_path(zarr_path)

  exists_attribute_files <- .file_or_blob_exists(
    zarr_path,
    s3_client,
    c(".zattrs", "zarr.json")
  )

  if (!any(exists_attribute_files)) {
    stop(
      "No file that could contain attributes (either `.zattrs` for v2 ",
      "or `zarr.json` for v3) was found in the path.",
      call. = FALSE
    )
  }
  if (all(exists_attribute_files)) {
    stop(
      "The path contains both `.zattrs` (v2 Zarr specification) ",
      "and `zarr.json` (v3 Zarr specification) files, which is not allowed.",
      call. = FALSE
    )
  }
  attribute_file <- names(exists_attribute_files)[exists_attribute_files]
  attribute_path <- paste0(
    zarr_path,
    attribute_file
  )

  if (!is.null(s3_client)) {
    parsed_url <- parse_s3_path(attribute_path)

    s3_object <- s3_client$get_object(
      Bucket = parsed_url$bucket,
      Key = parsed_url$object
    )

    zattrs <- fromJSON(rawToChar(s3_object$Body))
  } else {
    zattrs <- read_json(attribute_path)
  }

  if (attribute_file == "zarr.json") {
    zattrs <- zattrs[["attributes"]]
  }

  # Normalize cases of empty named list vs empty list vs NULL
  if (length(zattrs) == 0) {
    return(list())
  }

  return(zattrs)
}
