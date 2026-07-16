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

test_that("out of order indices are handled correctly", {
  # Used in ZarrArray for example.
  # See https://github.com/Huber-group-EMBL/Rarr/issues/211
  zarr_v2 <- system.file(
    package = "Rarr",
    "extdata",
    "zarr_examples",
    "column-first",
    "int32.zarr"
  )
  # Non-compact indices
  expect_no_condition(
    res1 <- read_zarr_array(zarr_v2, list(c(3, 1), c(2, 1), c(1, 3)))
  )
  expect_shape(res1, dim = c(2, 2, 2))
  expect_no_condition(
    res2 <- read_zarr_array(zarr_v2, list(c(1, 3), c(1, 2), c(3, 1)))
  )
  expect_identical(
    res1,
    res2[c(2, 1), c(2, 1), c(2, 1)]
  )

  # Compact index ranges
  expect_no_condition(
    res3 <- read_zarr_array(zarr_v2, list(3:1, 2:1, 1:3))
  )
  expect_shape(res3, dim = c(3, 2, 3))
  expect_no_condition(
    res4 <- read_zarr_array(zarr_v2, list(1:3, 1:2, 3:1))
  )
  expect_identical(
    res3,
    res4[c(3, 2, 1), c(2, 1), c(3, 2, 1)]
  )
})
