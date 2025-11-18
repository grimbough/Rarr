test_that("Anonymous S3 access is detected properly", {
  path <- "https://www.test.com/bucket/file1"
  s3_client <- .create_s3_client(path)

  ## This ensures .get_credentials always returns an error, even if the
  ## host machine has credentials available
  with_mocked_bindings(
    .get_credentials = function(...) stop(),
    expect_true(s3_client$.internal$config$credentials$anonymous)
  )
})

test_that("Authenticated S3 access works", {
  skip_if_offline("s3.embl.de")

  withr::with_envvar(
    c(
      "AWS_ACCESS_KEY_ID" = "bYUBYVg1AsEreuDgtg5K",
      "AWS_SECRET_ACCESS_KEY" = "r8FrLXc9dseD6V1P3htsu7ZBzP7Gszsd3sM1G4KX"
    ),
    {
      expect_snapshot(zarr_overview(
        "https://s3.embl.de/rarr-testing/bzip2.zarr"
      ))
      expect_snapshot(read_zarr_array(
        "https://s3.embl.de/rarr-testing/bzip2.zarr"
      ))
    }
  )
})

test_that("Denied access errors return clear error messages", {
  skip_if_offline("s3.embl.de")

  # This is a real zarr store, but we don't have access
  expect_error(
    zarr_overview("https://s3.embl.de/rarr-testing/lzma.zarr"),
    "Denied"
  )
})
