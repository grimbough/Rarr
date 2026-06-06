test_that("blosc works with variable length types", {
  blosc_vlen_zarr <- withr::local_tempfile(fileext = ".zarr")

  x <- matrix(
    c("Paris", "London", "Berlin", "Madrid"),
    nrow = 2
  )
  # TODO: update this to the canonical way to write vlen-utf8 once we have it.
  # It's this way only because the vlen-utf8 interface has not been created yet.
  create_empty_zarr_array(
    blosc_vlen_zarr,
    dim = dim(x),
    chunk_dim = c(2, 2),
    data_type = "|O",
    compressor = use_blosc()
  )
  zarr_json_path <- file.path(blosc_vlen_zarr, "zarr.json")
  zarr_json <- jsonlite::read_json(zarr_json_path)
  zarr_json$data_type <- "string"
  # There should be only one bytes-array codec
  zarr_json$codecs <- lapply(
    zarr_json$codecs,
    function(codec) {
      if (codec$name == "bytes") {
        list(name = "vlen-utf8", configuration = list())
      } else {
        codec
      }
    }
  )
  jsonlite::write_json(
    zarr_json,
    zarr_json_path,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )

  expect_no_condition(
    update_zarr_array(blosc_vlen_zarr, x, index = list(1:2, 1:2))
  )

  expect_identical(
    read_zarr_array(blosc_vlen_zarr),
    x
  )
})

test_that("error when setting up compressors with invalid level", {
  expect_error(
    use_blosc(clevel = 30),
    "`clevel` value must be an integer between 0 and 9"
  )
  expect_error(
    use_zstd(level = 30),
    "`level` value must be an integer between 0 and 22"
  )
})
