test_that("Inf fill-values are understood correctly", {
  zarr <- system.file(
    "extdata",
    "zarr_examples",
    "fill-values",
    "double-inf.zarr",
    package = "Rarr"
  )

  fill_val <- Inf
  expect_silent(data <- read_zarr_array(zarr, index = list(1:2, 1:10)))

  # The first row should all be 1
  expect_identical(data[1, ], rep(1, 10))

  # Second row is Inf
  expect_equal(data[2, ], rep(fill_val, 10))

  # Read a chunk that isn't on disk, only generated from fill value
  expect_silent(data2 <- read_zarr_array(zarr, index = list(19:20, 1:10)))
  expect_equal(as.vector(data2), rep(fill_val, length(data2)))
})

test_that("-Inf fill-values are understood correctly", {
  zarr <- system.file(
    "extdata",
    "zarr_examples",
    "fill-values",
    "double-neginf.zarr",
    package = "Rarr"
  )

  fill_val <- -Inf
  expect_silent(data <- read_zarr_array(zarr, index = list(1:2, 1:10)))

  # The first row should all be 1
  expect_identical(data[1, ], rep(1, 10))

  # Second row is -Inf
  expect_equal(data[2, ], rep(fill_val, 10))

  # Read a chunk that isn't on disk, only generated from fill value
  expect_silent(data2 <- read_zarr_array(zarr, index = list(19:20, 1:10)))
  expect_equal(as.vector(data2), rep(fill_val, length(data2)))
})
