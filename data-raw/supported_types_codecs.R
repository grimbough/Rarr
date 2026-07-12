CODEC_BYTES_BYTES <- c(
  "blosc",
  "zlib",
  "gzip",
  "bz2",
  "lzma",
  "numcodecs.lz4",
  "lz4",
  "zstd"
)
CODEC_ARRAY_ARRAY <- c("transpose")
CODEC_ARRAY_BYTES <- c("bytes", "vlen-utf8", "sharding_indexed")

SUPPORTED_V3_TYPES <- list(
  "bool" = list(base_type = "bool", nbytes = 1L),
  "int8" = list(base_type = "int", nbytes = 1L),
  "int16" = list(base_type = "int", nbytes = 2L),
  "int32" = list(base_type = "int", nbytes = 4L),
  "int64" = list(base_type = "int", nbytes = 8L),
  "uint8" = list(base_type = "uint", nbytes = 1L),
  "uint16" = list(base_type = "uint", nbytes = 2L),
  "uint32" = list(base_type = "uint", nbytes = 4L),
  "uint64" = list(base_type = "uint", nbytes = 8L),
  "float16" = list(base_type = "float", nbytes = 2L),
  "float32" = list(base_type = "float", nbytes = 4L),
  "float64" = list(base_type = "float", nbytes = 8L),
  "string" = list(base_type = "string", nbytes = NA_integer_)
) |>
  list2env(hash = TRUE, parent = emptyenv())

usethis::use_data(
  SUPPORTED_V3_TYPES,
  CODEC_BYTES_BYTES,
  CODEC_ARRAY_ARRAY,
  CODEC_ARRAY_BYTES,
  internal = TRUE,
  overwrite = TRUE
)
