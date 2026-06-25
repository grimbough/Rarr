test_that("read sharded files", {
  sharded <- system.file(
    "extdata",
    "zarr_examples",
    "sharding",
    "int32_sharded_v3.zarr",
    package = "Rarr"
  )

  arr <- read_zarr_array(sharded) |>
    expect_no_condition() |>
    expect_shape(dim = c(30L, 20L, 10L)) |>
    expect_type("integer")

  expect_identical(
    arr[1L, , 1L],
    seq_len(20L)
  )
  expect_identical(
    arr[, 1L, 1L],
    rep_len(1L, 30L)
  )

  expect_identical(
    arr[2L:30L, 2L:20L, 2L:10L],
    array(0L, dim = c(29L, 19L, 9L))
  )
})
