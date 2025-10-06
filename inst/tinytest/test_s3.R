path <- "https://www.test.com/bucket/file1"
s3_client <- .create_s3_client(path)

expect_error(
  zarr_overview("https://s3.embl.de/rarr-testing/bzip2.zarr"),
  "Denied"
)

if (require(mockery)) {
  ## This ensures .get_credentials always returns an error, even if the
  ## host machine has credentials available
  stub(
    where = .create_s3_client,
    what = '.get_credentials',
    how = function(...) stop(),
    depth = 2
  )

  ## we expect an "anonymous" credential if nothing is found
  expect_true(s3_client$.internal$config$credentials$anonymous)
}

test_file_s3_url <- "https://noaa-nwm-retro-v2-zarr-pds.s3.amazonaws.com/feature_id"
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
