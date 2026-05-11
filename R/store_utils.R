.create_store <- function(path, s3_client = NULL) {
  zarr_path <- .normalize_array_path(path)
  if (any(startsWith(path, c("s3://", "http://", "https://")))) {
    if (is.null(s3_client)) {
      s3_path <- parse_s3_path(path)
      store <- robstore::s3_store_anonymous(
        bucket = s3_path$bucket,
        region = s3_path$region,
        endpoint = s3_path$hostname,
        allow_http = startsWith(path, "http://")
      )
    } else {
      s3_path <- parse_s3_path(path)
      store <- robstore::s3_store(
        bucket = s3_path$bucket,
        region = s3_path$region,
        endpoint = s3_path$hostname,
        allow_http = startsWith(path, "http://"),
      )
    }
  } else {
    store <- robstore::local_store(path)
  }

  return(store)
}
