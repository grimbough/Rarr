test_that("uint8 zarr arrays can be read correctly", {
  zarr_c <- system.file(
    "extdata",
    "zarr_examples",
    "column-first",
    "uint8.zarr",
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
  expect_identical(column_major[1, , ], c(-1L, 2:20))
  ## first column should be all 1s
  expect_identical(column_major[, 1, ], rep(-1L, 30))
})

test_that("v2 and v3 return identical results", {
  zarr_v2 <- system.file(
    "extdata",
    "zarr_examples",
    "column-first",
    "uint8.zarr",
    package = "Rarr"
  )
  zarr_v3 <- system.file(
    "extdata",
    "zarr_examples",
    "column-first",
    "uint8_v3.zarr",
    package = "Rarr"
  )

  expect_no_condition(u8_v2 <- read_zarr_array(zarr_v2))
  expect_no_condition(u8_v3 <- read_zarr_array(zarr_v3))

  expect_identical(u8_v2, u8_v3)
})
