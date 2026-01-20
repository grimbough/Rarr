test_that("An empty zarr array with dimension 0 can be created", {
  # This is used in anndataR for example
  empty_zarr <- withr::local_tempfile(fileext = ".zarr")
  expect_no_condition(
    Rarr::create_empty_zarr_array(
      empty_zarr,
      dim = 0,
      chunk_dim = 0,
      data_type = "integer"
    )
  )
  empty_metadata <- Rarr::zarr_overview(empty_zarr, as_data_frame = TRUE)
  expect_identical(empty_metadata$dim, list(0L))
  expect_identical(empty_metadata$chunk_dim, list(0L))
})
