test_that("uint16 zarr arrays can be read correctly", {
  zarr_c <- system.file(
    "extdata",
    "zarr_examples",
    "column-first",
    "uint16.zarr",
    package = "Rarr"
  )
  index <- list(1:30, 1:20, 1)

  ## Shouldn't be any warnings for this input
  expect_silent(column_major <- read_zarr_array(zarr_c, index = index))

  ## we return an array
  expect_true(is.array(column_major))

  ## dimensions equal to the index
  expect_identical(dim(column_major), c(30L, 20L, 1L))

  ## first row should be sequence 1 to 20
  expect_identical(column_major[1, , ], c(1:20))
  ## first column should be all 1s
  expect_identical(column_major[, 1, ], rep(1L, 30))
})

test_that("v2 and v3 return identical results", {
  zarr_v2 <- system.file(
    "extdata",
    "zarr_examples",
    "column-first",
    "uint16.zarr",
    package = "Rarr"
  )
  zarr_v3 <- system.file(
    "extdata",
    "zarr_examples",
    "column-first",
    "uint16_v3.zarr",
    package = "Rarr"
  )

  expect_no_condition(u16_v2 <- read_zarr_array(zarr_v2))
  expect_no_condition(u16_v3 <- read_zarr_array(zarr_v3))

  expect_identical(u16_v2, u16_v3)
})

test_that("uint16 zarr array can be written", {
  uint16_zarr <- withr::local_tempfile(fileext = ".zarr")

  content_uint16_array <- array(1:24, dim = c(4, 3, 2))

  write_zarr_array(
    content_uint16_array,
    uint16_zarr,
    data_type = "<u2",
    chunk_dim = c(2, 2, 1),
    compressor = NULL
  )

  roundtrip_uint16_array <- read_zarr_array(uint16_zarr)

  expect_identical(
    content_uint16_array,
    roundtrip_uint16_array
  )
})
