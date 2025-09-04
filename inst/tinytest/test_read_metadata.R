zarr_c <- system.file(
  "extdata",
  "zarr_examples",
  "column-first",
  "int64.zarr",
  package = "Rarr"
)

## return results as a data.frame
df <- zarr_overview(zarr_c, as_data_frame = TRUE)
expect_inherits(df, "data.frame")
expect_equal(dim(df), c(1, 6))

## write details to screen
withr::with_tempfile("zarr_overview_array", {
  withr::with_output_sink(
    zarr_overview_array,
    zarr_overview(zarr_c, as_data_frame = FALSE)
  )
  actual <- readLines(zarr_overview_array)
  expected <- readLines(system.file(
    "tinytest",
    "snapshots",
    "zarr_overview_array.txt",
    package = "Rarr"
  ))
  expect_identical(
    # "Path" is absolute path so will differ between systems
    actual[!startsWith(actual, "Path: ")],
    expected[!startsWith(expected, "Path: ")]
  )
})


zarr_store_consolidated <- system.file(
  "extdata",
  "zarr_examples",
  "metadata",
  "consolidated.zarr",
  package = "Rarr"
)

df <- zarr_overview(zarr_store_consolidated, as_data_frame = TRUE)
expect_inherits(df, "data.frame")
expect_equal(dim(df), c(3, 6))
expect_identical(
  colnames(df),
  c("path", "nchunks", "data_type", "compressor", "dim", "chunk_dim")
)

withr::with_tempfile("zarr_overview_store", {
  withr::with_output_sink(
    zarr_overview_store,
    zarr_overview(zarr_store_consolidated, as_data_frame = FALSE)
  )
  actual <- readLines(zarr_overview_store)
  expected <- readLines(system.file(
    "tinytest",
    "snapshots",
    "zarr_overview_store.txt",
    package = "Rarr"
  ))
  expect_identical(
    # "Path" is absolute path so will differ between systems
    actual[!grepl("^\\s*Path: ", actual)],
    expected[!grepl("^\\s*Path: ", actual)]
  )
})

## v3 metadata
zarr_v3 <- system.file(
  "extdata",
  "zarr_examples",
  "metadata",
  "v3.zarr",
  package = "Rarr"
)
df <- zarr_overview(zarr_v3, as_data_frame = TRUE)
expect_inherits(df, "data.frame")
expect_equal(dim(df), c(1, 6))

withr::with_tempfile("zarr_overview_v3", {
  withr::with_output_sink(
    zarr_overview_v3,
    zarr_overview(zarr_v3, as_data_frame = FALSE)
  )
  actual <- readLines(zarr_overview_v3)
  expected <- readLines(system.file(
    "tinytest",
    "snapshots",
    "zarr_overview_v3.txt",
    package = "Rarr"
  ))
  expect_identical(
    # "Path" is absolute path so will differ between systems
    actual[!startsWith(actual, "Path: ")],
    expected[!startsWith(expected, "Path: ")]
  )
})

## error when metadata file is not found (local zarr array)
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

## error when metadata file is not found (zarr array on s3)
if (require(mockery)) {
  # Ensures that .s3_object_exists() just automatically returns FALSE
  stub(
    where = Rarr:::.read_array_metadata,
    what = ".s3_object_exists",
    how = FALSE
  )

  expect_error(
    zarr_overview(
      "http://s3.example.com/bucket/example.zarr/",
      s3_client = list()
    ),
    "The path does not contain any metadata files."
  )
}
