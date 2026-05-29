#' Print a summary of a Zarr array or group
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
zarr_overview <- function(
  zarr_array_path,
  s3_client = NULL,
  as_data_frame = FALSE
) {
  zarr_array_path <- .normalize_array_path(zarr_array_path)
  s3_client <- s3_client %||% .create_s3_client(path = zarr_array_path)

  dot_zmeta <- .read_consolidated_metadata(
    zarr_path = zarr_array_path,
    nodes = "array",
    s3_client = s3_client
  )
  if (!is.null(dot_zmeta)) {
    is_array <- vapply(
      dot_zmeta$metadata,
      function(x) !is.null(x$node_type) && x$node_type == "array",
      FUN.VALUE = logical(1L)
    )
    arrays <- names(dot_zmeta$metadata)[is_array]
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
    array_metadata <- .read_array_metadata(
      zarr_path = zarr_array_path,
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
  # FIXME: This is redundant with .read_array_metadata() but we didn't go
  # through it if we are dealing with consolidated metadata.
  names(array_metadata$codecs) <- vapply(
    array_metadata$codecs,
    function(x) x$name,
    character(1L)
  )

  chunk_shape <- unlist(array_metadata$chunk_grid$configuration$chunk_shape)
  data_shape <- unlist(array_metadata$shape)
  nchunks <- ceiling(
    data_shape / chunk_shape
  )

  codecs <- array_metadata$codecs
  compressor <- names(codecs)[match(
    TRUE,
    names(codecs) %in% CODEC_BYTES_BYTES
  )]
  endianness <- codecs[["bytes"]][["configuration"]][["endian"]]

  res <- data.frame(
    path = paste0(.normalize_array_path(zarr_array_path), array_name),
    data_type = if (is.list(array_metadata$data_type)) {
      array_metadata$data_type$name
    } else {
      array_metadata$data_type
    },
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
      character(1L)
    ),
    vapply(
      array_metadata_df$chunk_dim,
      function(x) {
        paste(unlist(x), collapse = " x ")
      },
      character(1L)
    ),
    vapply(
      array_metadata_df$nchunks,
      function(x) {
        chunks <- unlist(x)
        paste0(prod(chunks), " (", paste(chunks, collapse = " x "), ")")
      },
      character(1L)
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

#' Read the `.zarray` or `zarr.json` metadata file associated with a Zarr array
#'
#' @param zarr_path A character vector of length 1.  This provides the
#'   path to a Zarr array or group of arrays. This can either be on a local file
#'   system or on S3 storage.
#' @param s3_client A list representing an S3 client.  This should be produced
#'   by [paws.storage::s3()].
#' @param ... Temporary fix for backwards compatibility. Will be removed soon.
#'
#' @returns A list containing the array metadata
#'
#' @importFrom grumpy parse_npy_datatype
#'
#' @keywords internal
.read_array_metadata <- function(zarr_path, s3_client = NULL, ...) {
  # FIXME: remove ... argument after https://github.com/Bioconductor/ZarrArray/pull/7
  # is merged
  metadata_file <- c(".zarray", "zarr.json") |>
    .store_check_exist(zarr_path, files = _, s3_client = s3_client)

  if (metadata_file[".zarray"] && metadata_file["zarr.json"]) {
    stop(
      "The path contains both `.zarray` (Zarr V2 specification) and ",
      "`zarr.json` (Zarr V3 specification) metadata files.\n",
      "An array or group must conform to either the Zarr V2 or V3 ",
      "specification.",
      call. = FALSE
    )
  }
  if (!any(metadata_file)) {
    stop(
      "The path does not contain any metadata files. ",
      "It must contain one of:\n",
      "  - `.zarray` (Zarr V2 specification)\n",
      "  - `zarr.json` (Zarr V3 specification)",
      call. = FALSE
    )
  }

  metadata_path <- paste0(zarr_path, names(metadata_file)[metadata_file])

  metadata <- .read_json_file(metadata_path, s3_client)

  if (metadata$zarr_format == 2L) {
    ## if we do this here, we save many repeated calls to .parse_npy_datatype
    ## the parsed version is used each time a chunk is read
    metadata$datatype <- parse_npy_datatype(metadata$dtype)
    metadata <- .convert_metadata_version(
      metadata,
      version_from = 2L,
      version_to = 3L
    )
  } else if (metadata$node_type == "array" && metadata$zarr_format == 3L) {
    metadata <- .normalize_v3_metadata(metadata)
  }
  metadata$fill_value <- .update_fill_value(
    metadata$fill_value,
    metadata$datatype,
    metadata$data_type
  )
  if (
    is.list(metadata$data_type) &&
      metadata$data_type$name %in%
        c("fixed_length_utf32", "null_terminated_bytes")
  ) {
    # It is annoying to have to handle both strings & lists in `data_type` in
    # downstream code, so we convert the fixed-length string types to their v2
    # equivalents here.
    # Their length has been calculated in .parse_datatype_v3() and stored in nbytes.
    metadata$data_type <- switch(
      metadata$data_type$name,
      "fixed_length_utf32" = "unicode",
      "null_terminated_bytes" = "string"
    )
  }

  return(metadata)
}

#' Apply Zarr v3-specific fixups to raw array metadata
#'
#' Called after reading a `zarr.json` file for an array node.  Performs four
#' normalisation steps that are needed before the metadata can be used by the
#' rest of the package:
#'
#' 1. Parse the `data_type` string into an R-friendly `datatype` list.
#' 2. Name `codecs` by their `name` field for O(1) lookup.
#' 3. Normalise scalar arrays (empty `shape`) to shape `1`.
#' 4. Inject a default `transpose` codec when absent (R uses F-order so we
#'    always need to know the intended order).
#' 5. Replicate the `endian` field across all fields of struct datatypes so
#'    that downstream chunk-reading code can treat it uniformly.
#'
#' @param metadata A list as returned by [.read_json_file()] for a Zarr v3
#'   array node (i.e. `metadata$zarr_format == 3L` and
#'   `metadata$node_type == "array"`).
#'
#' @returns The modified `metadata` list.
#'
#' @keywords internal
.normalize_v3_metadata <- function(metadata) {
  metadata$datatype <- .parse_datatype_v3(metadata$data_type)

  # We shouldn't have any case where x$name is NULL since the v3 spec states
  # 'name' MUST be a plain string.
  names(metadata$codecs) <- vapply(
    metadata$codecs,
    function(x) x$name,
    character(1L)
  )

  if (length(metadata$shape) == 0L) {
    # Empty tuple in shape means we are dealing with a scalar.
    metadata$shape <- 1L
    metadata$chunk_grid <- list(
      name = "regular",
      configuration = list(chunk_shape = 1L)
    )
  }

  # This needs to happen after we address the scalar edge case
  if (is.null(metadata$codecs[["transpose"]])) {
    # We need to make sure this is always present because we do the
    # reverse of what this codec is telling us (since R uses F-order).
    # So even when using the implicit default, we need to add it here.
    metadata$codecs[["transpose"]] <- list(
      name = "transpose",
      configuration = list(order = seq_along(metadata$shape) - 1L)
    )
  }

  # Set endian to NA for 1-byte types; replicate across struct fields
  if (!is.null(metadata$codecs[["bytes"]])) {
    endian <- metadata$codecs[["bytes"]]$configuration$endian %||%
      NA_character_
    # In v3 struct datatypes, endian is defined only once for the whole array
    # but our reading infra (based on v2) expects it per base type.
    endian <- rep(endian, length(metadata$datatype$base_type))
    metadata$codecs[["bytes"]]$configuration$endian <- endian
    metadata$datatype$endian <- endian
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
#' @param fill_value The fill value as read from the Zarr metadata.
#' @param datatype A list of details for the array datatype.  Expected to be
#' produced by [grumpy::parse_npy_datatype()].
#'
#' @returns Returns a modified fill value.  The returned value will be equal
#'   to the input, but with the correct type, unless the `fill_value` entry was one
#'   of: NULL, "NaN", "Infinity" or "-Infinity".
#'
#' @keywords internal
#' @noRd
.update_fill_value <- function(fill_value, datatype, data_type) {
  if (length(datatype$base_type) > 1L) {
    if (data_type$name == "struct") {
      return(mapply(
        function(fill, bt, endian, nbytes) {
          .update_fill_value(
            fill,
            list(base_type = bt, endian = endian, nbytes = nbytes)
          )
        },
        fill_value,
        datatype$base_type,
        datatype$endian,
        datatype$nbytes
      ))
    }
    if (data_type$name == "structured") {
      decoded_fill_value <- jsonlite::base64_dec(fill_value)
      fill_value <- vector("list", length(datatype$base_type))
      for (i in seq_along(datatype$base_type)) {
        el_fill_value <- decoded_fill_value[seq_len(datatype$nbytes[i])]
        fill_value[[i]] <- grumpy::convert_bytes_to_array(
          el_fill_value,
          datatype$base_type[i],
          shape = NULL,
          datatype$nbytes[i],
          "little"
        )
        decoded_fill_value <- decoded_fill_value[-seq_len(datatype$nbytes[i])]
      }
      return(fill_value)
    }
  }
  ## a null fill value implies no missing values.
  ## We set to NA as you can't create an array of NULL in R
  if (is.null(fill_value)) {
    fill_value <- NA
  } else if (fill_value %in% c("NaN", "Infinity", "-Infinity")) {
    if (datatype$base_type != "string") {
      fill_value <- switch(
        fill_value,
        "NaN" = NaN,
        "Infinity" = Inf,
        "-Infinity" = -Inf
      )
    }
  } else if (is.numeric(fill_value)) {
    fill_value <- switch(
      datatype$base_type,
      "float" = as.double(fill_value),
      "int" = as.integer(fill_value),
      "uint" = as.integer(fill_value),
      "complex" = as.complex(fill_value),
      "string" = as.character(fill_value),
      "unicode" = as.character(fill_value),
      fill_value
    )
  } else if (datatype$base_type == "float" && startsWith(fill_value, "0x")) {
    # FIXME: the spec only defines this for floats but surely it makes
    # sense to also apply it to int and uint?
    hex_clean <- sub("^0x", "", fill_value)
    byte_pairs <- paste0(
      "0x",
      regmatches(hex_clean, gregexpr(".{2}", hex_clean))[[1L]]
    )
    # Swap endianness
    endian <- switch(
      datatype$endian,
      "little" = "big",
      "big" = "little"
    )
    fill_value <- readBin(
      as.raw(byte_pairs),
      what = "double",
      size = datatype$nbytes,
      endian = endian
    )
  }
  return(fill_value)
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
#' @importFrom grumpy parse_npy_datatype
#'
#' @keywords internal
.read_consolidated_metadata <- function(
  zarr_path,
  nodes = c("group", "array"),
  s3_client = NULL
) {
  s3_client <- s3_client %||% .create_s3_client(zarr_path)

  metadata_file <- .store_check_exist(
    zarr_path,
    files = c(".zmetadata", "zarr.json"),
    s3_client
  )
  if (!metadata_file[".zmetadata"] && !metadata_file["zarr.json"]) {
    return(NULL)
  }
  if (metadata_file[".zmetadata"] && metadata_file["zarr.json"]) {
    stop(
      "The path contains both `.zmetadata` (Zarr V2 specification) and ",
      "`zarr.json` (Zarr V3 specification) consolidated metadata files.\n",
      "An array or group must conform to either the Zarr V2 or V3 ",
      "specification.",
      call. = FALSE
    )
  }

  zmeta_path <- paste0(zarr_path, names(metadata_file)[metadata_file])

  # At this stage, we are sure the file exists
  zmeta <- .read_json_file(zmeta_path, s3_client)

  if (metadata_file[".zmetadata"]) {
    arrays <- names(zmeta$metadata)[endsWith(
      names(zmeta$metadata),
      "/.zarray"
    )]
    zmeta$metadata[arrays] <- lapply(
      zmeta$metadata[arrays],
      function(metadata) {
        metadata$datatype <- parse_npy_datatype(metadata$dtype)
        .convert_metadata_version(
          metadata,
          version_from = 2L,
          version_to = 3L
        )
      }
    )
  } else {
    if (zmeta$node_type == "array") {
      return(NULL)
    }
    zmeta <- zmeta$consolidated_metadata
  }
  if (length(zmeta$metadata) == 0L) {
    warning(
      "The consolidated metadata file was found but was empty. ",
      "Consider filling it with `zarr_consolidate_metadata()`.",
      call. = FALSE
    )
  }
  return(zmeta)
}

#' Read the attributes associated with a Zarr array or group
#'
#' @inheritParams .read_array_metadata
#' @param missing A character vector of length 1. This determines the behaviour
#'   when no file containing attributes is found. This can be one of:
#'  - "ignore" (the default): an empty list is returned silently
#'  - "warning": a warning is issued and an empty list is returned.
#'  - "error": an error is raised.
#'
#' @returns A list containing the attributes. If the file containing attributes
#' (`.zattrs` for Zarr v2 or `zarr.json` for Zarr v3) exists but no attributes
#' are provided, an empty list is returned.
#'
#' @examples
#' read_zarr_attributes(
#'   "https://uk1s3.embassy.ebi.ac.uk/idr/zarr/v0.4/idr0048A/9846152.zarr"
#' )
#'
#' @export
read_zarr_attributes <- function(
  zarr_path,
  s3_client = NULL,
  missing = c("ignore", "warning", "error")
) {
  missing <- match.arg(missing)
  zarr_path <- .normalize_array_path(zarr_path)
  ## determine if this is a local or S3 array
  s3_client <- s3_client %||% .create_s3_client(path = zarr_path)

  exists_attribute_files <- .store_check_exist(
    zarr_path,
    c(".zattrs", "zarr.json"),
    s3_client
  )

  if (!any(exists_attribute_files)) {
    msg <- paste(
      "No file that could contain attributes",
      "(either `.zattrs` for v2 or `zarr.json` for v3)",
      "was found in the path."
    )
    if (missing == "error") {
      stop(msg, call. = FALSE)
    }
    if (missing == "warning") {
      warning(msg, call. = FALSE)
    }
    return(list())
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

  zattrs <- .read_json_file(attribute_path, s3_client)

  if (attribute_file == "zarr.json") {
    zattrs <- zattrs[["attributes"]]
  }

  # Normalize cases of empty named list vs empty list vs NULL
  if (length(zattrs) == 0L) {
    return(list())
  }

  return(zattrs)
}
