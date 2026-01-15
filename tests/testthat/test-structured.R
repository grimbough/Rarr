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
