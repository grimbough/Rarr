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
with_mocked_bindings(
  code = {
    expect_error(
      zarr_overview("https://objects.eodc.eu:443/e05ab01a9d56408d82ac32d69a5aae2a:202505-s02msil2a/30/products/cpm_v256/S2B_MSIL2A_20250530T101559_N0511_R065_T32TPT_20250530T130924.zarr/measurements/reflectance/r10m/"),
      "The array does not have an associated .zarray metadata file"
    )
  },
  # Since the s3 client is never actually used (because .s3_object_exists() just
  # returns FALSE), just need to make sure the client is not NULL, so the
  # correct flow occurs in read_array_metadata()
  .create_s3_client = function(path) {
    list()
  },
  .s3_object_exists = function(s3_client, Bucket, Key) {
    FALSE
  }
)
