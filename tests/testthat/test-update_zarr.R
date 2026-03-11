test_that("update_zarr_array replaces chunks for 1D arrays", {
  path <- withr::local_tempfile(fileext = ".zarr")
  res <- create_empty_zarr_array(
    zarr_array_path = path,
    dim = 100,
    chunk_dim = 10,
    data_type = "integer",
    fill_value = 100L,
    zarr_version = 2
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

  path_v3 <- withr::local_tempfile(fileext = ".zarr")
  res_v3 <- create_empty_zarr_array(
    zarr_array_path = path_v3,
    dim = 100,
    chunk_dim = 10,
    data_type = "integer",
    fill_value = 100L,
    zarr_version = 3
  )

  x <- array(1:5, dim = 5)
  expect_true(update_zarr_array(path_v3, x = x, index = list(1:5)))
  expect_identical(
    read_zarr_array(path_v3),
    array(c(1:5, rep(100L, 95)), dim = 100)
  )
  # only a single chunk file should have been created
  expect_identical(list.files(path_v3, recursive = TRUE), c("c/0", "zarr.json"))

  x <- rep(20L, 5)
  expect_true(update_zarr_array(
    path_v3,
    x = x,
    index = list(c(91, 93, 95, 97, 99))
  ))
  expect_identical(
    read_zarr_array(path_v3)[91:100],
    array(rep(c(20L, 100L), 5), dim = 10)
  )
  # expect two chunk files were created
  expect_identical(
    list.files(path_v3, recursive = TRUE),
    c("c/0", "c/9", "zarr.json")
  )
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

test_that("update higher precision from lower precision internal type", {
  path <- withr::local_tempfile(fileext = ".zarr")
  res <- create_empty_zarr_array(
    zarr_array_path = path,
    dim = c(10, 10),
    chunk_dim = c(5, 5),
    data_type = "<i8",
    fill_value = 0L
  )

  x <- matrix(1:100, ncol = 10)

  expect_true(update_zarr_array(path, x, index = list(NULL, NULL)))
  expect_identical(read_zarr_array(path), x)
})


test_that("update unsigned array", {
  path <- withr::local_tempfile(fileext = ".zarr")
  res <- create_empty_zarr_array(
    zarr_array_path = path,
    dim = c(10, 10),
    chunk_dim = c(5, 5),
    data_type = "<u4",
    fill_value = 0L
  )

  x <- matrix(1:100, ncol = 10)

  expect_true(update_zarr_array(path, x, index = list(NULL, NULL)))
  expect_identical(read_zarr_array(path), x)
})

test_that("update VLen-UTF8 array", {
  path <- withr::local_tempfile(fileext = ".zarr")
  dir.create(path)
  list(
    shape = list(12L, 12L),
    chunks = list(6L, 6L),
    dtype = "|O",
    fill_value = "",
    order = "C",
    filters = list(list(id = "vlen-utf8")),
    dimension_separator = ".",
    compressor = list(id = "zstd", level = 0L),
    zarr_format = 2L
  ) |>
    jsonlite::write_json(
      file.path(path, ".zarray"),
      auto_unbox = TRUE,
      pretty = TRUE
    )

  x <- rep_len("ça marche", 12L)

  expect_true(update_zarr_array(path, x, index = list(1, NULL)))
  res <- read_zarr_array(path)
  expect_identical(res[1, ], x)
  expect_identical(res[2:12, ], array("", dim = c(11, 12)))
})

test_that("update v3 array", {
  path <- withr::local_tempfile(fileext = ".zarr")
  dir.create(path)
  list(
    shape = list(30L, 20L, 10L),
    data_type = "int32",
    chunk_grid = list(
      name = "regular",
      configuration = list(chunk_shape = list(10L, 10L, 5L))
    ),
    chunk_key_encoding = list(
      name = "default",
      configuration = list(separator = "/")
    ),
    fill_value = 0L,
    codecs = list(
      list(name = "bytes", configuration = list(endian = "little")),
      list(name = "zstd", configuration = list(level = 0L, checksum = FALSE))
    ),
    zarr_format = 3L,
    node_type = "array"
  ) |>
    jsonlite::write_json(
      file.path(path, "zarr.json"),
      auto_unbox = TRUE,
      pretty = TRUE
    )

  x <- 1:30

  expect_true(update_zarr_array(path, x, index = list(NULL, 1, 1)))
  res <- read_zarr_array(path)
  expect_identical(res[, 1, 1], x)
  expect_identical(res[, 1:20, 2:10], array(0L, dim = c(30, 20, 9)))
})
