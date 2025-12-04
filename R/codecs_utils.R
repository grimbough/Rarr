#' Convert the metadata `codecs` elements into functions
#'
#' @param codecs A list containing the Zarr v3 codecs
#' @param operation One of "encode" or "decode"
#'
#' @returns An list of 3 environments containing functions:
#' - `bytes_bytes_codecs`: functions to encode/decode raw bytes
#' - `array_array_codecs`: functions to encode/decode R arrays
#' - `array_bytes_codecs`: functions to encode/decode between R arrays and raw bytes
.configure_codecs <- function(codecs, operation = c("encode", "decode")) {
  codecs_names <- names(codecs)
  operation <- match.arg(operation)

  bytes_bytes_codecs <- intersect(codecs_names, NULL)
  array_array_codecs <- intersect(codecs_names, "transpose")
  array_bytes_codecs <- intersect(codecs_names, c("endian", "vlen_utf8"))

  if (length(array_bytes_codecs) > 1) {
    stop("Only one array-bytes codec is supported at a time.")
  }

  bytes_bytes_env <- list()
  array_array_env <- list()
  array_bytes_env <- list()

  if ("transpose" %in% array_array_codecs) {
    # R is already F ordered, so we reverse the order in config
    cfg <- rev(unlist(codecs$transpose$configuration$order))
    if (is.unsorted(cfg)) {
      array_array_env[["transpose"]] <- switch(
        operation,
        "encode" = function(x) codec_transpose_encode(x, cfg + 1),
        "decode" = function(x) codec_transpose_decode(x, cfg + 1)
      )
    }
  }

  return(
    list(
      array_array = array_array_env,
      bytes_bytes = bytes_bytes_env,
      array_bytes = array_bytes_env
    )
  )
}
