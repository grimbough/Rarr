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

test_that("NA fill-values are understood correctly", {
  zarr_na_fill <- withr::local_tempfile(fileext = ".zarr")

  create_empty_zarr_array(
    zarr_na_fill,
    dim = c(4, 10),
    chunk_dim = c(2, 5),
    data_type = "double",
    fill_value = NA_real_
  )

  expect_true(
    is.na(.read_array_metadata(zarr_na_fill, "zarr.json")$fill_value)
  )

  index <- list(2:3, 2:6)
  res <- read_zarr_array(zarr_na_fill, index = index)

  expect_true(
    all(is.na(res))
  )
  expect_type(res, "double")

  update_zarr_array(
    zarr_na_fill,
    matrix(10.5, nrow = 2, ncol = 5),
    index
  )
  res <- read_zarr_array(zarr_na_fill)
  expect_type(res, "double")
  expect_identical(res[1, 1], NA_real_)
  expect_identical(res[2, 2], 10.5)
})

test_that("fill-values on empty slices", {
  # https://github.com/Huber-group-EMBL/Rarr/issues/137
  m <- matrix(runif(30), c(5, 6))
  my_zarr3 <- withr::local_tempfile(fileext = ".zarr")
  write_zarr_array(m, zarr_array_path = my_zarr3, chunk_dim = c(2, 3))

  expect_type(
    read_zarr_array(my_zarr3, list(1:2, integer(0))),
    "double"
  )
})
