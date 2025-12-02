test_that("uint64 zarr arrays can be read correctly", {
  zarr_c <- system.file(
    "extdata",
    "zarr_examples",
    "column-first",
    "uint64.zarr",
    package = "Rarr"
  )
  index <- list(1:30, 1:20, 1)

  # Shouldn't be any warnings for this input
  expect_silent(column_major <- read_zarr_array(zarr_c, index = index))

  # we return an array
  expect_true(is.array(column_major))

  # dimensions equal to the index
  expect_identical(dim(column_major), c(30L, 20L, 1L))

  # first row should be sequence 1 to 20
  expect_identical(column_major[1, , ], 1:20)
  # first column should be all 1s
  expect_identical(column_major[, 1, ], rep(1L, 30))
})


test_that("uint64 zarr arrays produce NA for out-of-range values", {
  zarr_c <- system.file(
    "extdata",
    "zarr_examples",
    "column-first",
    "uint64.zarr",
    package = "Rarr"
  )

  # this data point should be outside the range of an int32 and throw a warning
  expect_warning(
    column_major <- read_zarr_array(zarr_c, index = list(30L, 20L, 10L))
  )
  expect_true(all(is.na(column_major)))
})

test_that("v2 and v3 return identical results", {
  zarr_v2 <- system.file(
    "extdata",
    "zarr_examples",
    "column-first",
    "uint64.zarr",
    package = "Rarr"
  )
  zarr_v3 <- system.file(
    "extdata",
    "zarr_examples",
    "column-first",
    "uint64_v3.zarr",
    package = "Rarr"
  )

  u64_v2 <- read_zarr_array(zarr_v2)
  u64_v3 <- read_zarr_array(zarr_v3)

  expect_identical(u64_v2, u64_v3)
})
