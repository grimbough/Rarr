codec_blosc_decode <- function(bytes, ...) {
  .Call(
    "decompress_chunk_BLOSC",
    bytes,
    PACKAGE = "Rarr"
  )
}

codec_gzip_decode <- function(bytes, ...) {
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

codec_lzma_decode <- function(bytes, buffer_size, ...) {
  memDecompress(
    from = bytes,
    type = "xz",
    asChar = FALSE
  )
}

codec_lz4_decode <- function(bytes, buffer_size, ...) {
  ## numpy codecs stores the original size of the buffer in the first 4 bytes
  .Call(
    "decompress_chunk_LZ4",
    tail(x = bytes, n = -4L),
    buffer_size,
    PACKAGE = "Rarr"
  )
}

codec_zstd_decode <- function(bytes, buffer_size, ...) {
  .Call(
    "decompress_chunk_ZSTD",
    bytes,
    buffer_size,
    PACKAGE = "Rarr"
  )
}
