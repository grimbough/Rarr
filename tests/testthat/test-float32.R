test_that("float32 zarr arrays can be read correctly", {
  zarr_c <- system.file(
    "extdata",
    "zarr_examples",
    "column-first",
    "float32.zarr",
    package = "Rarr"
  )
  zarr_f <- system.file(
    "extdata",
    "zarr_examples",
    "row-first",
    "float32.zarr",
    package = "Rarr"
  )
  index <- list(1:30, 1:20, 1)

  # Shouldn't be any warnings for this input
  expect_silent(column_major <- read_zarr_array(zarr_c, index = index))
  expect_silent(row_major <- read_zarr_array(zarr_f, index = index))

  # Row and column major should be read the same in R
  expect_identical(column_major, row_major)

  # We return an array
  expect_true(is.array(column_major))

  # Dimensions equal to the index
  expect_identical(dim(column_major), lengths(index))

  # First row should be 2 to 20 except the first element which is 10.52
  expect_identical(column_major[1, , ], c(10.52, 2:20), tolerance = 1e-7)

  # First column should be all 10.52
  expect_identical(column_major[, 1, ], rep(10.52, 30), tolerance = 1e-7)
})
