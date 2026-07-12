#' Normalize a Zarr array path
#'
#' Taken from https://zarr.readthedocs.io/en/stable/spec/v2.html#logical-storage-paths
#'
#' @param path Character vector of length 1 giving the path to be normalised.
#'
#' @returns A character vector of length 1 containing the normalised path.
#'
#' @importFrom R.utils getAbsolutePath
#'
#' @keywords internal
.normalize_array_path <- function(path) {
  ## we strip the protocol because it gets messed up by the slash removal later
  supported_protocols <- c("http://", "https://", "s3://")
  if (any(startsWith(path, supported_protocols))) {
    root <- supported_protocols[startsWith(path, supported_protocols)]
  } else {
    ## Replace all backward slash ("\\") with forward slash ("/")
    path <- gsub(x = path, pattern = "\\", replacement = "/", fixed = TRUE)
    path <- getAbsolutePath(path, expandTilde = TRUE)
    root <- sub(x = path, "(^[[:alnum:]:.]*/)?(.*)", replacement = "\\1")
  }
  path <- substring(path, nchar(root) + 1L)

  ## Strip any leading "/" characters
  if (startsWith(path, "/")) {
    path <- substring(path, 2L)
  }
  ## Collapse any sequence of more than one "/" character into a single "/"
  path <- gsub(x = path, pattern = "//+", replacement = "/", fixed = FALSE)
  ## The key prefix is then obtained by appending a single "/" character to
  ## the normalized logical path.
  if (endsWith(path, "/")) {
    path <- paste0(root, path)
  } else {
    path <- paste0(root, path, "/")
  }

  return(path)
}

#' @importFrom stats setNames
.store_check_exist <- function(
  zarr_array_path,
  files,
  s3_client
) {
  if (is.null(s3_client)) {
    is_present <- setNames(
      file.exists(paste0(zarr_array_path, files, recycle0 = TRUE)),
      files
    )
  } else {
    parse_url <- parse_s3_path(zarr_array_path)
    is_present <- vapply(
      files,
      FUN = function(f) {
        key <- paste0(parse_url$object, f)
        .s3_object_exists(s3_client, parse_url$bucket, key)
      },
      FUN.VALUE = logical(1L)
    )
  }

  return(is_present)
}

# For backward compatibility with ZarrArray.
# Removed after https://github.com/Bioconductor/ZarrArray/pull/7 is merged.
.file_or_blob_exists <- function(path, s3_client, files) {
  .store_check_exist(path, files, s3_client)
}

.store_list <- function(path, recursive, s3_client) {
  if (is.null(s3_client)) {
    list.files(
      path,
      recursive = recursive,
      include.dirs = TRUE,
      all.files = TRUE
    )
  } else {
    s3_client$list_objects(path, recursive = recursive)
  }
}

#' Read a JSON file from local disk or S3
#'
#' @param path Full path (local or S3) to a JSON file.
#' @param s3_client An S3 client produced by [paws.storage::s3()], or `NULL`
#'   for local files.
#'
#' @returns A list as returned by [jsonlite::read_json()] /
#'   [jsonlite::fromJSON()].
#'
#' @importFrom jsonlite read_json fromJSON
#'
#' @keywords internal
.read_json_file <- function(path, s3_client = NULL) {
  if (!is.null(s3_client)) {
    parsed_url <- parse_s3_path(path)
    s3_object <- s3_client$get_object(
      Bucket = parsed_url$bucket,
      Key = parsed_url$object
    )
    # simplifyVector = FALSE is used for consistency with read_json(),
    # used on local files.
    fromJSON(rawToChar(s3_object$Body), simplifyVector = FALSE)
  } else {
    read_json(path)
  }
}
