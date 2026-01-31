test_that("check_index errors for mismatched dimensions", {
  expect_error(
    check_index(index = list(1), metadata = list(shape = c(10, 10)))
  )
})


test_that("check_index errors for negative indices", {
  expect_error(
    check_index(index = list(-1, 1), metadata = list(shape = c(10, 10)))
  )
})


test_that("check_index errors for indices outside extent", {
  expect_error(
    check_index(index = list(100, 1), metadata = list(shape = c(10, 10)))
  )
})

test_that("integer(0) indexing works correctly", {
  zarr_v2 <- system.file(
    package = "Rarr",
    "extdata",
    "zarr_examples",
    "column-first",
    "int32.zarr"
  )
  expect_no_condition(
    res <- read_zarr_array(zarr_v2, list(integer(0), integer(0), integer(0)))
  )
  expect_shape(res, dim = c(0, 0, 0))

  zarr_v3 <- system.file(
    package = "Rarr",
    "extdata",
    "zarr_examples",
    "column-first",
    "int32_v3.zarr"
  )
  expect_no_condition(
    res <- read_zarr_array(zarr_v3, list(integer(0), integer(0), integer(0)))
  )
  expect_shape(res, dim = c(0, 0, 0))
})
