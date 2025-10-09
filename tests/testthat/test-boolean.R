test_that("boolean zarr array can be read correctly", {
  zarr_c <- system.file(
    "extdata",
    "zarr_examples",
    "column-first",
    "boolean.zarr",
    package = "Rarr"
  )
  index <- list(1:5, NULL)

  # Shouldn't be any warnings for this input
  expect_silent(column_major <- read_zarr_array(zarr_c, index = index))

  # we return an array
  expect_true(is.array(column_major))

  # dimensions equal to the index
  expect_identical(dim(column_major), c(5L, 10L))

  # first row should be TRUE
  # second row should be FALSE
  expect_true(all(column_major[1, ]))
  expect_false(any(column_major[2, ]))
})

test_that("boolean zarr array can be written", {
  bool_zarr <- withr::local_tempfile(fileext = ".zarr")

  content_bool_array <- matrix(c(TRUE, FALSE, TRUE, FALSE), nrow = 2)
  write_zarr_array(
    content_bool_array,
    bool_zarr,
    chunk_dim = c(2, 2),
    compressor = NULL
  )

  roundtrip_bool_array <- read_zarr_array(bool_zarr)

  expect_identical(
    content_bool_array,
    roundtrip_bool_array
  )

})
