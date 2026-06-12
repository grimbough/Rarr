#' Convert the metadata `codecs` elements into functions
#'
#' @param codecs A list containing the Zarr v3 codecs
#' @param operation One of "encode" or "decode"
#'
#' @returns An list of 3 environments containing functions:
#' - `bytes_bytes_codecs`: functions to encode/decode raw bytes
#' - `array_array_codecs`: functions to encode/decode R arrays
#' - `array_bytes_codecs`: functions to encode/decode between R arrays and raw bytes
#'
#' @keywords internal
.configure_codecs <- function(codecs, operation = c("encode", "decode")) {
  codecs_names <- names(codecs)
  operation <- match.arg(operation)

  bytes_bytes_codecs <- intersect(codecs_names, CODEC_BYTES_BYTES)
  array_array_codecs <- intersect(codecs_names, CODEC_ARRAY_ARRAY)
  array_bytes_codecs <- intersect(codecs_names, CODEC_ARRAY_BYTES)

  unsupported_codecs <- setdiff(
    codecs_names,
    c(
      bytes_bytes_codecs,
      array_array_codecs,
      array_bytes_codecs
    )
  )
  if (length(unsupported_codecs) > 0L) {
    stop(
      "The following codecs are not supported: ",
      toString(unsupported_codecs)
    )
  }

  bytes_bytes_env <- list()
  array_array_env <- list()
  array_bytes_env <- list()

  if ("transpose" %in% array_array_codecs) {
    # R is already F ordered, so we reverse the order in config
    cfg <- rev(unlist(codecs$transpose$configuration$order)) + 1L
    if (is.unsorted(cfg)) {
      array_array_env[["transpose"]] <- switch(
        operation,
        "encode" = eval(bquote(function(x) {
          codec_transpose_encode(x, .(cfg))
        })),
        "decode" = eval(bquote(function(x) {
          codec_transpose_decode(x, .(cfg))
        }))
      )
    }
  }

  for (candidate_codec in array_bytes_codecs) {
    cfg <- codecs[[candidate_codec]]$configuration %||% NA_character_
    func_name <- paste("codec", candidate_codec, operation, sep = "_")
    array_bytes_env[[candidate_codec]] <- eval(bquote(function(...) {
      do.call(.(func_name), c(list(...), .(cfg)))
    }))
  }

  # Compressors
  for (candidate_codec in bytes_bytes_codecs) {
    if (candidate_codec %in% codecs_names) {
      cfg <- codecs[[candidate_codec]]$configuration
      # Aliases
      candidate_codec <- switch(
        candidate_codec,
        "numcodecs.lz4" = "lz4",
        candidate_codec
      )
      func_name <- paste("codec", candidate_codec, operation, sep = "_")
      bytes_bytes_env[[candidate_codec]] <- eval(bquote(function(bytes) {
        do.call(.(func_name), c(list(bytes), .(cfg)))
      }))
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
