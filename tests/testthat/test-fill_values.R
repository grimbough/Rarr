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

test_that("fill-values on empty slices", {
  # https://github.com/Huber-group-EMBL/Rarr/issues/137
  m <- matrix(runif(30), c(5, 6))
  my_zarr3 <- tempfile(fileext = ".zarr")
  write_zarr_array(m, zarr_array_path = my_zarr3, chunk_dim = c(2, 3))

  expect_type(
    read_zarr_array(my_zarr3, list(1:2, integer(0))),
    "double"
  )
})
