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

test_that("float32 zarr arrays can be written", {
  f32_zarr <- withr::local_tempfile(fileext = ".zarr")

  content_f32_array <- array(runif(24), dim = c(4, 3, 2))

  write_zarr_array(
    content_f32_array,
    f32_zarr,
    data_type = "<f4",
    chunk_dim = c(2, 2, 1),
    compressor = NULL
  )

  roundtrip_f32_array <- read_zarr_array(f32_zarr)

  # Conversion to float32 leads to loss of precision
  expect_equal(
    content_f32_array,
    roundtrip_f32_array,
    tolerance = 1e-7
  )
})