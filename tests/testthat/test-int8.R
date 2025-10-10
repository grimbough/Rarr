test_that("int8 zarr arrays can be read correctly", {
  zarr_c <- system.file(
    "extdata",
    "zarr_examples",
    "column-first",
    "int8.zarr",
    package = "Rarr"
  )
  zarr_f <- system.file(
    "extdata",
    "zarr_examples",
    "row-first",
    "int8.zarr",
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
  expect_identical(dim(column_major), c(30L, 20L, 1L))

  # First row should be sequence 1 to 20
  expect_identical(column_major[1, , ], 1:20)

  # First column should be all 1s
  expect_identical(column_major[, 1, ], rep(1L, 30))
})

test_that("i8 zarr array can be written", {
  i8_zarr <- withr::local_tempfile(fileext = ".zarr")

  content_i8_array <- array(1:24, dim = c(4, 3, 2))

  write_zarr_array(
    content_i8_array,
    i8_zarr,
    data_type = "|i1",
    chunk_dim = c(2, 2, 1),
    compressor = NULL
  )

  roundtrip_i8_array <- read_zarr_array(i8_zarr)

  expect_identical(
    content_i8_array,
    roundtrip_i8_array
  )
})

test_that("overflow when writing cause warning", {
  overflowing_zarr <- withr::local_tempfile(fileext = ".zarr")

  content_overflowing_array <- array(128L, c(3, 3, 3))

  expect_warning(
    write_zarr_array(
      content_overflowing_array,
      overflowing_zarr,
      data_type = "|i1",
      chunk_dim = c(3, 3, 3),
      compressor = NULL
    ),
    "truncate"
  )

  expect_identical(
    read_zarr_array(overflowing_zarr),
    array(127L, c(3, 3, 3))
  )

  overflowing_zarr2 <- withr::local_tempfile(fileext = ".zarr")
  content_overflowing_array2 <- array(1000L, c(3, 3, 3))

  expect_warning(
    write_zarr_array(
      content_overflowing_array2,
      overflowing_zarr2,
      data_type = "|i1",
      chunk_dim = c(3, 3, 3),
      compressor = NULL
    ),
    "truncate"
  )

  expect_identical(
    read_zarr_array(overflowing_zarr2),
    array(127L, c(3, 3, 3))
  )

  negative_overflowing_zarr <- withr::local_tempfile(fileext = ".zarr")
  content_negative_overflowing_array <- array(-1000L, c(3, 3, 3))

  expect_warning(
    write_zarr_array(
      content_negative_overflowing_array,
      negative_overflowing_zarr,
      data_type = "|i1",
      chunk_dim = c(3, 3, 3),
      compressor = NULL
    ),
    "truncate"
  )

  expect_identical(
    read_zarr_array(negative_overflowing_zarr),
    array(-128L, c(3, 3, 3))
  )
})
