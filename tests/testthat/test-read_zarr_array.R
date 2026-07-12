test_that("read_zarr_array() gives an informative error on v3 groups", {
  zarr_group <- withr::local_tempdir(fileext = ".zarr")

  jsonlite::write_json(
    list(
      zarr_format = 3L,
      node_type = "group"
    ),
    file.path(zarr_group, "zarr.json"),
    auto_unbox = TRUE
  )
  expect_error(
    read_zarr_array(zarr_group),
    "The provided path points to a Zarr group, but `read_zarr_array()` can only read arrays.",
    fixed = TRUE
  )
})
