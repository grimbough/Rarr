test_zarr_file <- system.file(
  "extdata",
  "zarr_examples",
  "row-first",
  "other.zarr",
  package = "Rarr"
)

expect_silent(
  test_array <- read_zarr_array(test_zarr_file)
)

expect_identical(
  test_array,
  array(as.character(0:5))
)
