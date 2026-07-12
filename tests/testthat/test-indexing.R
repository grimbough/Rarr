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

  # Edge case from https://github.com/Huber-group-EMBL/Rarr/issues/112#issuecomment-3842837618.
  # This happens because the scalar case is special-cased.
  zarr_scalar_v3 <- system.file(
    package = "Rarr",
    "extdata",
    "zarr_examples",
    "scalar",
    "scalar_v3.zarr"
  )
  expect_no_condition(
    res <- read_zarr_array(zarr_scalar_v3, list(integer(0)))
  )
  expect_identical(res, integer(0), ignore_attr = "dim")
})
