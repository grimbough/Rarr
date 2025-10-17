test_that("update_zarr_array replaces chunks for 1D arrays", {
  path <- withr::local_tempfile(fileext = ".zarr")
  res <- create_empty_zarr_array(
    zarr_array_path = path,
    dim = 100,
    chunk_dim = 10,
    data_type = "integer",
    fill_value = 100L
  )

  x <- array(1:5, dim = 5)
  expect_true(update_zarr_array(path, x = x, index = list(1:5)))
  expect_identical(
    read_zarr_array(path),
    array(c(1:5, rep(100L, 95)), dim = 100)
  )
  # only a single chunk file should have been created
  expect_identical(list.files(path), "0")

  x <- rep(20L, 5)
  expect_true(update_zarr_array(
    path,
    x = x,
    index = list(c(91, 93, 95, 97, 99))
  ))
  expect_identical(
    read_zarr_array(path)[91:100],
    array(rep(c(20L, 100L), 5), dim = 10)
  )
  # expect two chunk files were created
  expect_length(list.files(path), 2)
})

test_that("update_zarr_array handles NULL in index for some dimensions (2D case)", {
  path <- withr::local_tempfile(fileext = ".zarr")
  res <- create_empty_zarr_array(
    zarr_array_path = path,
    dim = c(20, 20),
    chunk_dim = c(10, 10),
    data_type = "integer",
    fill_value = 0L
  )
  x <- matrix(1:40L, ncol = 2)
  index <- list(NULL, 1:2)

  expect_true(update_zarr_array(path, x = x, index = index))
  expect_identical(read_zarr_array(path, index = index), x)
})

test_that("update_zarr_array on C-order arrays", {
  path <- withr::local_tempfile(fileext = ".zarr")
  res <- create_empty_zarr_array(
    zarr_array_path = path,
    dim = c(20, 20),
    chunk_dim = c(10, 10),
    data_type = "integer",
    fill_value = 0L,
    order = "C",
    compressor = NULL
  )

  x <- matrix(1:400, ncol = 20)

  expect_true(update_zarr_array(path, x, index = list(NULL, NULL)))
  expect_identical(read_zarr_array(path), x)
})
