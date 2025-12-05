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
  zarr_v2 <- system.file(
    "extdata",
    "zarr_examples",
    "compression",
    "lz4.zarr",
    package = "Rarr"
  )
  zarr_v3 <- system.file(
    "extdata",
    "zarr_examples",
    "compression",
    "lz4_v3.zarr",
    package = "Rarr"
  )

  expect_silent(data_v2 <- read_zarr_array(zarr_v2))
  expect_silent(data_v3 <- read_zarr_array(zarr_v3))

  expect_identical(data_v2, data_v3)

  expect_identical(data_v2[1, ], 1:10)
  expect_true(all(data_v2[, 1] == seq(1, 60, 3)))
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

  # Decompression of unknown buffer size
  # https://github.com/Huber-group-EMBL/Rarr/issues/20
  zarr_zstd_vlen <- system.file(
    "extdata",
    "zarr_examples",
    "compression",
    "zstd_vlen.zarr",
    package = "Rarr"
  )

  expect_silent(data_vlen <- read_zarr_array(zarr_zstd_vlen))

  expect_identical(data_vlen, array(as.character(0:5), dim = 6))
})
