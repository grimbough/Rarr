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

test_that("v3 structured data types", {
  v3_structured_file <- system.file(
    "extdata",
    "zarr_examples",
    "structured",
    "structured_v3.zarr",
    package = "Rarr"
  )

  expect_silent(
    res <- read_zarr_array(v3_structured_file)
  )

  expect_shape(res, dim = c(10, 2))
  dim(res) <- NULL
  for (el in res) {
    expect_length(el, 2L)
    # FIXME: support names in structured datatypes
    # expect_named(el, c("x", "y"))
    expect_type(el[[1]], "integer")
    expect_type(el[[2]], "double")
  }
})
