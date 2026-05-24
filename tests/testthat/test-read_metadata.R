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

  # From https://objects.eodc.eu:443/e05ab01a9d56408d82ac32d69a5aae2a:202505-s02msil2a/30/products/cpm_v256/S2B_MSIL2A_20250530T101559_N0511_R065_T32TPT_20250530T130924.zarr
  complex_consolidated <- system.file(
    "extdata",
    "zarr_examples",
    "metadata",
    "complex_consolidated.zarr",
    package = "Rarr"
  )
  metadata_df <- zarr_overview(complex_consolidated, as_data_frame = TRUE)
  # "path" is absolute path so will differ between systems
  metadata_df$path <- gsub(
    complex_consolidated,
    "",
    metadata_df$path,
    fixed = TRUE
  )
  expect_snapshot(
    metadata_df
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

test_that("zarr_overview works with consolidated v3 metadata", {
  zarr_store_consolidated_v3 <- system.file(
    "extdata",
    "zarr_examples",
    "metadata",
    "consolidated_v3.zarr",
    package = "Rarr"
  )
  expect_snapshot(
    zarr_overview(zarr_store_consolidated_v3, as_data_frame = FALSE),
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

test_that("zarr_overview doesn't choke on empty consolidated metadata", {
  zarr_empty_consolidated <- system.file(
    "extdata",
    "zarr_examples",
    "metadata",
    "empty_consolidated.zarr",
    package = "Rarr"
  )
  zarr_overview(zarr_empty_consolidated) |>
    expect_warning("The consolidated metadata file was found but was empty.")
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

test_that("zarr_overview throws error for mixed v2/v3 zarr", {
  invalid_zarr <- system.file(
    "extdata",
    "zarr_examples",
    "metadata",
    "invalid_mixed.zarr",
    package = "Rarr"
  )

  expect_snapshot_error(
    zarr_overview(invalid_zarr)
  )
})

test_that("missing metadata on S3 results in a clear error", {
  with_mocked_bindings(
    expect_error(
      zarr_overview(
        "http://s3.example.com/bucket/example.zarr/",
        list()
      ),
      "The path does not contain any metadata files."
    ),
    .s3_object_exists = function(...) FALSE
  )
})
