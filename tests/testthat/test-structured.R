test_that("scalar structured data types", {
  common_type_file <- system.file(
    "extdata",
    "zarr_examples",
    "structured",
    "common_type.zarr",
    package = "Rarr"
  )

  expect_silent(
    res <- read_zarr_array(common_type_file)
  )

  expect_shape(res, dim = c(10, 10))
})

test_that("unicode structured data types", {
  unicode_type_file <- system.file(
    "extdata",
    "zarr_examples",
    "structured",
    "unicode.zarr",
    package = "Rarr"
  )

  expect_silent(
    res <- read_zarr_array(unicode_type_file)
  )

  expect_shape(res, dim = c(10, 10))
})
