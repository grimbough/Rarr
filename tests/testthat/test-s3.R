test_that("Anonymous S3 access is detected properly", {
  skip_if_not_installed("mockery")

  path <- "https://www.test.com/bucket/file1"
  s3_client <- .create_s3_client(path)

  ## This ensures .get_credentials always returns an error, even if the
  ## host machine has credentials available
  mockery::stub(
    where = .create_s3_client,
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
