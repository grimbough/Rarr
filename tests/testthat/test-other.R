test_that("other zarr arrays can be read correctly", {
  test_zarr_file <- system.file(
    "extdata",
    "zarr_examples",
    "row-first",
    "other.zarr",
    package = "Rarr"
  )

  # Shouldn't be any warnings for this input
  expect_silent(
    test_array <- read_zarr_array(test_zarr_file)
  )

  # Should return an array of character values from 0 to 5
  expect_identical(
    test_array,
    array(as.character(0:5))
  )
})
