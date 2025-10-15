test_that("compressed zarr arrays (bzip2) can be read correctly", {
  zarr <- system.file(
    "extdata",
    "zarr_examples",
    "compression",
    "bzip2.zarr",
    package = "Rarr"
  )

  expect_silent(data <- read_zarr_array(zarr))

  expect_identical(data[1, ], 1:10)
  expect_true(all(data[, 1] == seq(1, 60, 3)))
})

test_that("compressed zarr arrays (lz4) can be read correctly", {
  zarr <- system.file(
    "extdata",
    "zarr_examples",
    "compression",
    "lz4.zarr",
    package = "Rarr"
  )

  expect_silent(data <- read_zarr_array(zarr))

  expect_identical(data[1, ], 1:10)
  expect_true(all(data[, 1] == seq(1, 60, 3)))
})

test_that("compressed zarr arrays (lzma) can be read correctly", {
  zarr <- system.file(
    "extdata",
    "zarr_examples",
    "compression",
    "lzma.zarr",
    package = "Rarr"
  )

  expect_silent(data <- read_zarr_array(zarr))

  expect_identical(data[1, ], 1:10)
  expect_true(all(data[, 1] == seq(1, 60, 3)))
})

test_that("compressed zarr arrays (zlib) can be read correctly", {
  zarr <- system.file(
    "extdata",
    "zarr_examples",
    "compression",
    "zlib.zarr",
    package = "Rarr"
  )

  expect_silent(data <- read_zarr_array(zarr))

  expect_identical(data[1, ], 1:10)
  expect_true(all(data[, 1] == seq(1, 60, 3)))
})

test_that("compressed zarr arrays (zstd) can be read correctly", {
  zarr <- system.file(
    "extdata",
    "zarr_examples",
    "compression",
    "zstd.zarr",
    package = "Rarr"
  )

  expect_silent(data <- read_zarr_array(zarr))

  expect_identical(data[1, ], 1:10)
  expect_true(all(data[, 1] == seq(1, 60, 3)))
})
