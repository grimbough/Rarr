.convert_metadata_version <- function(metadata, version_from, version_to) {
  if (version_from != 2 || version_to != 3) {
    stop(
      "Only conversion from version 2 to version 3 is supported.",
      call. = FALSE
    )
  }

  dt <- .parse_datatype(metadata$dtype)

  list(
    shape = metadata$shape,
    chunk_grid = list(
      name = "regular",
      configuration = list(chunk_shape = metadata$chunks)
    ),
    chunk_key_encoding = list(
      name = "default",
      configuration = list(
        separator = metadata$dimension_separator
      )
    ),
    data_type = paste0(dt$base_type, 8 * dt$nbytes),
    fill_value = metadata$fill_value,
    codecs = list(
      list(
        name = "bytes",
        configuration = c("endian" = dt$endian)
      ),
      list(
        name = metadata$compressor$id,
        configuration = list(
          metadata$compressor[names(metadata$compressor) != "id"]
        )
      ),
      list(
        name = "transpose",
        configuration = list(
          order = switch(
            metadata$order,
            "F" = seq_along(metadata$shape) - 1, # zero indexed
            "C" = rev(seq_along(metadata$shape)) - 1
          )
        )
      )
      # TODO: add filters
    )
  )
}
