test_that("creation of deeply nested group", {
  zarr_v2 <- withr::local_tempfile(fileext = ".zarr")
  expect_no_condition(
    write_zarr_group(zarr_v2, "test/deep/nested/group", zarr_version = 2L)
  )
  expect_true(dir.exists(file.path(zarr_v2, "test/deep/nested/group")))
  expect_identical(
    list.files(zarr_v2, recursive = TRUE, all.files = TRUE),
    c(
      ".zgroup",
      "test/.zgroup",
      "test/deep/.zgroup",
      "test/deep/nested/.zgroup",
      "test/deep/nested/group/.zgroup"
    )
  )
})
