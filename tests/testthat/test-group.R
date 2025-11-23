library(testthat)

test_that("create zarr and groups", {
  td <- withr::local_tempdir()
  name <- "test"
  output_zarr <- file.path(td, paste0(name, ".zarr"))

  # open zarr
  # create_zarr(dir = td, prefix = name)
  create_zarr(store = output_zarr)
  expect_true(dir.exists(output_zarr))
  expect_true(file.exists(file.path(output_zarr, ".zgroup")))

  # create group one group
  create_zarr_group(store = output_zarr, name = "group1")
  expect_true(dir.exists(file.path(output_zarr, "group1")))
  expect_true(file.exists(file.path(output_zarr, "group1", ".zgroup")))

  # create nested two groups
  create_zarr_group(store = output_zarr, name = "group2/subgroup1")
  expect_true(dir.exists(file.path(output_zarr, "group2")))
  expect_true(file.exists(file.path(output_zarr, "group2", ".zgroup")))
  expect_true(dir.exists(file.path(output_zarr, "group2/subgroup1")))
  expect_true(file.exists(file.path(
    output_zarr,
    "group2/subgroup1",
    ".zgroup"
  )))

  # create nested three groups
  create_zarr_group(store = output_zarr, name = "group3/subgroup1/subsubgroup1")
  expect_true(dir.exists(file.path(output_zarr, "group3")))
  expect_true(file.exists(file.path(output_zarr, "group3", ".zgroup")))
  expect_true(dir.exists(file.path(output_zarr, "group3/subgroup1")))
  expect_true(file.exists(file.path(
    output_zarr,
    "group3/subgroup1",
    ".zgroup"
  )))
  expect_true(dir.exists(file.path(
    output_zarr,
    "group3/subgroup1/subsubgroup1"
  )))
  expect_true(file.exists(file.path(
    output_zarr,
    "group3/subgroup1/subsubgroup1",
    ".zgroup"
  )))
})

test_that("create_zarr rejects unsupported version", {
  td <- withr::local_tempdir()
  name <- "test"
  output_zarr <- file.path(td, paste0(name, ".zarr"))
  expect_error(
    create_zarr(store = output_zarr, version = "v4"),
    # create_zarr(dir = td, prefix = name, version = "v4"),
    "only zarr v2 is supported"
  )
})
