test_that("zarr_overview returns data.frame for single array", {
  zarr_c <- system.file(
    "extdata",
    "zarr_examples",
    "column-first",
    "int64.zarr",
    package = "Rarr"
  )

  # Return results as a data.frame
  df <- zarr_overview(zarr_c, as_data_frame = TRUE)
  expect_s3_class(df, "data.frame")
  expect_identical(dim(df), c(1L, 7L))
})

test_that("zarr_overview console output matches snapshot for single array", {
  zarr_c <- system.file(
    "extdata",
    "zarr_examples",
    "column-first",
    "int64.zarr",
    package = "Rarr"
  )

  expect_snapshot(
    zarr_overview(zarr_c, as_data_frame = FALSE),
    # "Path" is absolute path so will differ between systems
    transform = function(x) gsub("^Path: .*", "Path: <path>", x)
  )
})

test_that("zarr_overview works with consolidated metadata store", {
  zarr_store_consolidated <- system.file(
    "extdata",
    "zarr_examples",
    "metadata",
    "consolidated.zarr",
    package = "Rarr"
  )

  df <- zarr_overview(zarr_store_consolidated, as_data_frame = TRUE)
  expect_s3_class(df, "data.frame")
  expect_identical(dim(df), c(3L, 7L))
  expect_identical(
    colnames(df),
    c(
      "path",
      "data_type",
      "endianness",
      "compressor",
      "dim",
      "chunk_dim",
      "nchunks"
    )
  )
})

test_that("zarr_overview console output matches snapshot for consolidated store", {
  zarr_store_consolidated <- system.file(
    "extdata",
    "zarr_examples",
    "metadata",
    "consolidated.zarr",
    package = "Rarr"
  )

  expect_snapshot(
    zarr_overview(zarr_store_consolidated, as_data_frame = FALSE),
    # "Path" is absolute path so will differ between systems
    transform = function(x) gsub("^(  )?Path: .*", "Path: <path>", x)
  )
})

test_that("zarr_overview works with v3 metadata", {
  zarr_v3 <- system.file(
    "extdata",
    "zarr_examples",
    "metadata",
    "v3.zarr",
    package = "Rarr"
  )

  df <- zarr_overview(zarr_v3, as_data_frame = TRUE)
  expect_s3_class(df, "data.frame")
  expect_identical(dim(df), c(1L, 7L))
})

test_that("zarr_overview console output matches snapshot for v3 metadata", {
  zarr_v3 <- system.file(
    "extdata",
    "zarr_examples",
    "metadata",
    "v3.zarr",
    package = "Rarr"
  )

  expect_snapshot(
    zarr_overview(zarr_v3, as_data_frame = FALSE),
    # "Path" is absolute path so will differ between systems
    transform = function(x) gsub("^Path: .*", "Path: <path>", x)
  )
})

test_that("zarr_overview throws error for missing metadata files", {
  zarr_c <- system.file(
    "extdata",
    "zarr_examples",
    "metadata",
    "missing.zarr",
    package = "Rarr"
  )

  expect_error(
    zarr_overview(zarr_c),
    "The path does not contain any metadata files."
  )
})

test_that("missing metadata on S3 results in a clear error", {
  with_mocked_bindings(
    expect_error(
      zarr_overview(
        "http://s3.example.com/bucket/example.zarr/"
      ),
      "The path does not contain any metadata files."
    ),
    .s3_object_exists = function(...) FALSE
  )
})
