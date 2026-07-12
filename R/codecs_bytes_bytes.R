codec_blosc_decode <- function(bytes, ...) {
  .Call(
    "decompress_chunk_BLOSC",
    bytes,
    PACKAGE = "Rarr"
  )
}

codec_blosc_encode <- function(bytes, compressor_config, ...) {
  .Call(
    "compress_chunk_BLOSC",
    bytes,
    compressor_config$typesize,
    compressor_config$cname,
    compressor_config$clevel,
    which(
      compressor_config$shuffle == c("noshuffle", "shuffle", "bitshuffle")
    ) -
      1L,
    compressor_config$blocksize,
    PACKAGE = "Rarr"
  )
}

codec_zlib_decode <- codec_gzip_decode <- function(bytes, ...) {
  memDecompress(
    from = bytes,
    type = "gzip",
    asChar = FALSE
  )
}

codec_bz2_decode <- function(bytes, ...) {
  memDecompress(
    from = bytes,
    type = "bzip2",
    asChar = FALSE
  )
}

codec_lzma_decode <- function(bytes, ...) {
  memDecompress(
    from = bytes,
    type = "xz",
    asChar = FALSE
  )
}

#' @importFrom utils tail
codec_lz4_decode <- function(bytes, ...) {
  # numcodecs docs says:
  # The compressed output includes a 4-byte header storing the original size
  # of the decompressed data as a little-endian 32-bit integer.
  buffer_size <- readBin(bytes, "integer", n = 1L, size = 4L, endian = "little")
  .Call(
    "decompress_chunk_LZ4",
    tail(x = bytes, n = -4L),
    buffer_size,
    PACKAGE = "Rarr"
  )
}

codec_lz4_encode <- function(bytes, ...) {
  ## numpy stores the original size of the buffer in the first 4 bytes after
  ## compression. We should do that too for compatibility
  c(
    writeBin(length(bytes), raw(), size = 4L, endian = "little"),
    .Call("compress_chunk_LZ4", bytes, PACKAGE = "Rarr")
  )
}

codec_zstd_decode <- function(bytes, ...) {
  .Call(
    "decompress_chunk_ZSTD",
    bytes,
    PACKAGE = "Rarr"
  )
}

codec_zstd_encode <- function(bytes, compressor_config, ...) {
  .Call(
    "compress_chunk_ZSTD",
    bytes,
    as.integer(compressor_config$level),
    PACKAGE = "Rarr"
  )
}
