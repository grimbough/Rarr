test_that("zarr_consolidate_metadata() on v2", {
  zarr_v2 <- withr::local_tempfile(fileext = ".zarr")
  dir.create(zarr_v2)
  jsonlite::write_json(
    list("zarr_format" = 2L),
    file.path(zarr_v2, ".zgroup")
  )
  write_zarr_array(
    array(1:4, dim = c(2, 2)),
    file.path(zarr_v2, "array1"),
    chunk_dim = c(1, 2),
    zarr_version = 2L
  )
  write_zarr_array(
    array(c(3.14, 42.42, 12.96, 7.89), dim = c(2, 2)),
    file.path(zarr_v2, "array2"),
    chunk_dim = c(1, 2),
    zarr_version = 2L
  )
  write_zarr_attributes(
    file.path(zarr_v2, "array1"),
    list(description = "This is array 1")
  )
  expect_snapshot(
    zarr_consolidate_metadata(zarr_v2, action = "return"),
    # "Path" is absolute path so will differ between systems
    transform = function(x) gsub("^(  )?Path: .*", "Path: <path>", x)
  )
})

test_that("zarr_consoliate_metadata() on v3", {
  zarr_v3 <- withr::local_tempfile(fileext = ".zarr")
  dir.create(zarr_v3)
  jsonlite::write_json(
    list(
      zarr_format = 3L,
      node_type = "group"
    ),
    file.path(zarr_v3, "zarr.json"),
    auto_unbox = TRUE
  )
  write_zarr_array(
    array(1:4, dim = c(2, 2)),
    file.path(zarr_v3, "array1"),
    chunk_dim = c(1, 2),
    zarr_version = 3L
  )
  write_zarr_array(
    array(c(3.14, 42.42, 12.96, 7.89), dim = c(2, 2)),
    file.path(zarr_v3, "array2"),
    chunk_dim = c(1, 2),
    zarr_version = 3L
  )
  write_zarr_attributes(
    file.path(zarr_v3, "array1"),
    list(description = "This is array 1")
  )
  expect_snapshot(
    zarr_consolidate_metadata(zarr_v3, action = "return"),
    # "Path" is absolute path so will differ between systems
    transform = function(x) gsub("^(  )?Path: .*", "Path: <path>", x)
  )
})