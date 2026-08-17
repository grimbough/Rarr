test_that("bfloat16 can be read", {
  bfloat16_array <- system.file(
    "extdata",
    "zarr_examples",
    "column-first",
    "bfloat16.zarr",
    package = "Rarr"
  ) |>
    read_zarr_array() |>
    expect_no_condition()

  expect_shape(bfloat16_array, dim = c(4L, 6L))
  expect_equal(
    bfloat16_array,
    matrix(
      1:24,
      nrow = 4L,
      ncol = 6L,
      byrow = TRUE
    )
  )
})
