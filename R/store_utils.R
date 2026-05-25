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
