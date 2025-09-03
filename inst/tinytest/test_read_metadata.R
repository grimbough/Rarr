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
expect_stdout(zarr_overview(zarr_c, as_data_frame = FALSE))

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

## error when metadata file is not found (local zarr array)
zarr_c <- system.file(
  "extdata",
  "zarr_examples",
  "missing-metadata",
  "int64.zarr",
  package = "Rarr"
)

expect_error(
  zarr_overview(zarr_c),
  "The array does not have an associated .zarray metadata file"
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
    zarr_overview("http://s3.example.com/bucket/example.zarr/",
      s3_client = list()
    ),
    "The array does not have an associated .zarray metadata file"
  )
}
