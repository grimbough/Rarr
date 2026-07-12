test_that("An empty zarr array with dimension 0 can be created", {
  # This is used in anndataR for example
  empty_zarr <- withr::local_tempfile(fileext = ".zarr")
  expect_no_condition(
    Rarr::create_empty_zarr_array(
      empty_zarr,
      dim = 0,
      chunk_dim = 0,
      data_type = "integer",
      zarr_version = 2
    )
  )
  empty_metadata <- Rarr::zarr_overview(empty_zarr, as_data_frame = TRUE)
  expect_identical(empty_metadata$dim, list(0L))
  expect_identical(empty_metadata$chunk_dim, list(0L))

  empty_zarr_v3 <- withr::local_tempfile(fileext = ".zarr")
  expect_no_condition(
    Rarr::create_empty_zarr_array(
      empty_zarr_v3,
      dim = 0,
      chunk_dim = 0,
      data_type = "integer",
      zarr_version = 3
    )
  )
  empty_metadata_v3 <- Rarr::zarr_overview(empty_zarr_v3, as_data_frame = TRUE)
  expect_identical(empty_metadata_v3$dim, list(0L))
  expect_identical(empty_metadata_v3$chunk_dim, list(0L))
})

test_that("Empty zarr arrays have expected metadata", {
  empty_zarr <- withr::local_tempfile(fileext = ".zarr")
  Rarr::create_empty_zarr_array(
    empty_zarr,
    dim = c(10, 30),
    chunk_dim = c(5, 3),
    data_type = "integer",
    compressor = use_blosc(
      cname = "blosclz",
      clevel = 9L,
      shuffle = "bitshuffle",
      blocksize = 0L
    ),
    zarr_version = 2L
  )
  expect_snapshot(
    jsonlite::read_json(file.path(empty_zarr, ".zarray"))
  )
})

test_that("Creating an empty zarr array via `write_zarr_array()`", {
  empty_zarr <- withr::local_tempfile(fileext = ".zarr")
  expect_no_condition(
    write_zarr_array(
      array(5L, dim = c(20, 20, 20)),
      empty_zarr,
      chunk_dim = c(2, 5, 5),
      fill_value = 5L
    )
  )
  expect_identical(
    list.files(empty_zarr, recursive = TRUE),
    "zarr.json"
  )
})
