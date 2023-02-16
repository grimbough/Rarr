#' Print a summary of a Zarr array
#'
#' When reading a Zarr array using [read_zarr_array()] it is necessary to know
#' it's shape and size. `zarr_overview()` can be used to get a quick overview of
#' the array shape and contents, based on the .zarray metadata file each array
#' contains.
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
#' z2 <- "https://uk1s3.embassy.ebi.ac.uk/idr/zarr/v0.4/idr0101A/13457539.zarr/1"
#' zarr_overview(z2)
#'
#' @export
zarr_overview <- function(zarr_array_path, s3_client, as_data_frame = FALSE) {
  zarr_array_path <- .normalize_array_path(zarr_array_path)
  
  if(missing(s3_client)) 
    s3_client <- .create_s3_client(path = zarr_array_path)

  dot_zmeta <- .read_zmetadata(zarr_path = zarr_array_path, s3_client = s3_client)
  if (!is.null(dot_zmeta)) {
    arrays <- grep(names(dot_zmeta$metadata), pattern = "/.zarray", fixed = TRUE, value = TRUE)

    if (as_data_frame) {
      tmp <- lapply(arrays,
        FUN = .rbind_array_metadata,
        metadata = dot_zmeta$metadata,
        zarr_array_path = zarr_array_path
      )
      res <- do.call(rbind.data.frame, tmp)
      return(res)
    } else {
      cat("Type: Group of Arrays\n")
      cat("Path:", normalizePath(zarr_array_path, mustWork = FALSE), "\n")
      cat("Arrays:\n")
      for (a in arrays) {
        cat("---\n")
        .print_array_metadata(dirname(a), dot_zarray = dot_zmeta$metadata[[a]], indent = "  ")
      }
    }
    invisible(TRUE)
  } else {
    dot_zarray <- read_array_metadata(path = zarr_array_path, s3_client = s3_client)
    if (as_data_frame) {
      res <- .rbind_array_metadata(array_name = basename(zarr_array_path), metadata = dot_zarray, dirname(zarr_array_path))
      return(res)
    } else {
      cat("Type: Array\n")
      .print_array_metadata(zarr_array_path, dot_zarray = dot_zarray)
      invisible(TRUE)
    }
  }
}

.rbind_array_metadata <- function(array_name, metadata, zarr_array_path) {
  if (array_name %in% names(metadata)) {
    dot_zarray <- metadata[[array_name]]
    array_name <- dirname(array_name)
  } else {
    dot_zarray <- metadata
  }

  dt <- .parse_datatype(dot_zarray$dtype)
  nchunks <- ceiling(unlist(dot_zarray$shape) / unlist(dot_zarray$chunks))

  res <- data.frame(
    path       = paste0(.normalize_array_path(zarr_array_path), array_name),
    nchunks    = prod(nchunks),
    data_type  = paste0(dt$base_type, 8 * dt$nbytes),
    compressor = ifelse(is.null(dot_zarray$compressor), NA, dot_zarray$compressor$id)
  )
  res$dim <- list(unlist(dot_zarray$shape))
  res$chunk_dim <- list(unlist(dot_zarray$chunks))
  return(res)
}

.print_array_metadata <- function(zarr_array_path, dot_zarray, indent = "") {
  dt <- .parse_datatype(dot_zarray$dtype)
  nchunks <- ceiling(unlist(dot_zarray$shape) / unlist(dot_zarray$chunks))

  cat(indent, "Path: ", normalizePath(zarr_array_path, mustWork = FALSE), "\n", sep = "")
  cat(indent, "Shape: ", paste(unlist(dot_zarray$shape), collapse = " x "), "\n", sep = "")
  cat(indent, "Chunk Shape: ", paste(unlist(dot_zarray$chunks), collapse = " x "), "\n", sep = "")
  cat(indent, "No. of Chunks: ", prod(nchunks), " (", paste(nchunks, collapse = " x "), ")", "\n", sep = "")
  cat(indent, "Data Type: ", dt$base_type, 8 * dt$nbytes, "\n", sep = "")
  cat(indent, "Endianness: ", dt$endian, "\n", sep = "")
  if (is.null(dot_zarray$compressor)) {
    cat(indent, "Compressor: None\n", sep = "")
  } else {
    cat(indent, "Compressor: ", dot_zarray$compressor$id, "\n", sep = "")
  }
}


#' @importFrom jsonlite read_json fromJSON
#' @importFrom stringr str_extract str_remove
#'
#' @keywords Internal
read_array_metadata <- function(path, s3_client = NULL) {
  path <- .normalize_array_path(path)
  zarray_path <- paste0(path, ".zarray")

  if (!is.null(s3_client)) {

    parsed_url <- parse_s3_path(zarray_path)
    
    s3_object <- s3_client$get_object(Bucket = parsed_url$bucket, 
                                      Key = parsed_url$object)

    metadata <- fromJSON(rawToChar(s3_object$Body))
  } else {
    metadata <- read_json(zarray_path)
  }

  metadata <- update_fill_value(metadata)

  return(metadata)
}

#' Convert special fill values from strings to numbers
#'
#' Special case fill values (NaN, Inf, -Inf) are encoded as strings in the Zarra
#' metadata.  R will create arrays of type character if these are defined and
#' the chunk isn't present on disk. This function updates the fill value to be
#' R's representation of these special values, so numeric arrays are created.
#'
#' @param metadata A list containing the array metadata.  This should normally
#'   be generated by running `read_json()` on the `.zarray` file.
#'
#' @returns Returns a list with the same structure as the input.  The returned
#'   list will be identical to the input, unless the `fill_value` entry was on
#'   of: "NaN", "Infinity" or "-Infinity".
#'
#' @keywords Internal
update_fill_value <- function(metadata) {
  if (metadata$fill_value %in% c("NaN", "Infinity", "-Infinity")) {
    datatype <- .parse_datatype(metadata$dtype)
    if (datatype$base_type != "string") {
      metadata$fill_value <- switch(metadata$fill_value,
        "NaN" = NaN,
        "Infinity" = Inf,
        "-Infinity" = -Inf
      )
    }
  }
  return(metadata)
}

#' @import jsonlite
#' @importFrom aws.s3 object_exists
#' @importFrom utils capture.output
#' @keywords Internal
.read_zmetadata <- function(zarr_path, s3_client) {
  
  zarr_path <- .normalize_array_path(zarr_path)
  zmeta_path <- paste0(zarr_path, ".zmetadata")
  zmeta <- NULL

  if (!is.null(s3_client)) {
    parsed_url <- parse_s3_path(zmeta_path)
    if(.s3_object_exists(s3_client, parsed_url$bucket, parsed_url$object)) {
      s3_object <- s3_client$get_object(Bucket = parsed_url$bucket, 
                                        Key = parsed_url$object)

      zmeta <- fromJSON(rawToChar(s3_object$Body))
    }
  } else {
    if (file.exists(zmeta_path)) {
      zmeta <- read_json(zmeta_path)
    }
  }

  return(zmeta)
}
