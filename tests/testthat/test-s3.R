test_that("Anonymous S3 access is detected properly", {
  skip_if_not_installed("mockery")

  path <- "https://www.test.com/bucket/file1"
  s3_client <- Rarr:::.create_s3_client(path)

  ## This ensures .get_credentials always returns an error, even if the
  ## host machine has credentials available
  mockery::stub(
    where = Rarr:::.create_s3_client,
    what = '.get_credentials',
    how = function(...) stop(),
    depth = 2
  )

  ## we expect an "anonymous" credential if nothing is found
  expect_true(s3_client$.internal$config$credentials$anonymous)
})

test_that("Denied access errors return clear error messages", {
  skip_if_offline()

  # This is a real zarr store, but we don't have access
  expect_error(
    zarr_overview("https://s3.embl.de/rarr-testing/lzma.zarr"),
    "Denied"
  )
})

test_that("S3 zarr overview works", {
  skip_if_offline()

  # This is a real zarr store, but we don't have access
  test_file_s3_url <- "https://noaa-nwm-retro-v2-zarr-pds.s3.amazonaws.com/feature_id/.zarray"
  expect_identical(
    zarr_overview(test_file_s3_url, as_data_frame = TRUE),
    list2DF(list(
      path = test_file_s3_url,
      data_type = "int32",
      endianness = "little",
      compressor = "blosc",
      dim = list(2729077L),
      chunk_dim = list(2729077L),
      nchunks = list(1)
    ))
  )
})