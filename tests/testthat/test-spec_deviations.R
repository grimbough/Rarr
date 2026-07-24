# This file regroups the specific conscious spec deviations we have implemented

test_that("chunk_size can be 0", {
  # https://github.com/zarr-developers/zarr-specs/issues/378
  new_zarr_array <- tempfile(fileext = ".zarr")
  write_zarr_array(
    array(numeric(0)),
    zarr_array_path = new_zarr_array,
    chunk_dim = 0
  )
  expect_identical(
    read_zarr_array(new_zarr_array),
    array(numeric(0))
  )
})

test_that("dimension_names is written for v2", {
  # TODO: implement reading as well?
  # https://github.com/zarr-developers/zarr-specs/blame/fc7dd9c9beb5a50b87f9b08b00bf50fc0048482f/docs/v2/v2.0.rst#L91-L92
  x <- array(
    1:60,
    c(4, 5, 3),
    dimnames = list(
      x = as.numeric(1:4),
      y = as.numeric(1:5),
      c = c("r", "g", "b")
    )
  )
  z <- tempfile(fileext = ".zarr")
  write_zarr_array(
    x,
    zarr_array_path = z,
    chunk_dim = c(2, 5, 1),
    zarr_version = 2L
  )
  expect_identical(
    jsonlite::read_json(file.path(z, ".zarray"))$dimension_names |> unlist(),
    c("x", "y", "c")
  )
})
