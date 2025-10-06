test_that("int64 zarr arrays can be read correctly", {
  zarr_c <- system.file(
    "extdata",
    "zarr_examples",
    "column-first",
    "int64.zarr",
    package = "Rarr"
  )
  zarr_f <- system.file(
    "extdata",
    "zarr_examples",
    "row-first",
    "int64.zarr",
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
  expect_equal(dim(column_major), c(30, 20, 1))

  # First row should be sequence 1 to 20
  expect_equal(column_major[1, , ], 1:20)

  # First column should be all 1s
  expect_equal(column_major[, 1, ], rep(1, 30))
})

test_that("int64 zarr arrays handle out-of-range values", {
  zarr_c <- system.file(
    "extdata",
    "zarr_examples",
    "column-first",
    "int64.zarr",
    package = "Rarr"
  )
  zarr_f <- system.file(
    "extdata",
    "zarr_examples",
    "row-first",
    "int64.zarr",
    package = "Rarr"
  )

  # This data point should be outside the range of an int32 and throw a warning
  expect_warning(
    column_major <- read_zarr_array(zarr_c, index = list(30, 20, 10))
  )
  expect_warning(row_major <- read_zarr_array(zarr_f, index = list(30, 20, 10)))
  expect_true(all(is.na(column_major)))
  expect_true(all(is.na(row_major)))

  # This data point should be smaller than the range of an int32 and throw a warning
  expect_warning(
    column_major <- read_zarr_array(zarr_c, index = list(30, 20, 9))
  )
  expect_warning(row_major <- read_zarr_array(zarr_f, index = list(30, 20, 9)))
  expect_true(all(is.na(column_major)))
  expect_true(all(is.na(row_major)))
})

test_that("int64 v3 zarr arrays throw appropriate error", {
  zarr_v3 <- system.file(
    "extdata",
    "zarr_examples",
    "column-first",
    "int64_v3.zarr",
    package = "Rarr"
  )

  expect_error(
    read_zarr_array(zarr_v3),
    "Zarr v3 arrays"
  )
})
