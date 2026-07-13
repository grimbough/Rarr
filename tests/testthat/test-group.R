library(testthat)
library(jsonlite)

for (zarr_version in c(2L, 3L)) {
  test_that("create zarr and groups", {
    output_zarr <- tempfile(fileext = ".zarr")
    metadata_file <- ifelse(zarr_version == 2L, ".zgroup", "zarr.json")

    # open zarr
    create_zarr(zarr_path = output_zarr, zarr_version = zarr_version)
    expect_true(dir.exists(output_zarr))
    expect_true(file.exists(file.path(output_zarr, metadata_file)))
    if (zarr_version == 3L) {
      md <- read_json(file.path(output_zarr, metadata_file))
      expect_in("node_type", names(md))
    }

    # create group one group
    create_zarr_group(zarr_path = output_zarr, group = "group1")
    expect_true(dir.exists(file.path(output_zarr, "group1")))
    expect_true(file.exists(file.path(output_zarr, "group1", metadata_file)))

    # attempt to change the zarr_version of the group throws a warning
    opposite_zarr_version <- ifelse(zarr_version == 2L, 3L, 2L)
    opposite_metadata_file <- ifelse(
      metadata_file == ".zgroup",
      "zarr.json",
      ".zgroup"
    )
    expect_warning(
      create_zarr_group(
        zarr_path = output_zarr,
        group = "group_ver",
        zarr_version = opposite_zarr_version
      ),
      "Thus, version will be fixed to"
    )
    expect_true(dir.exists(file.path(output_zarr, "group_ver")))
    md <- read_json(file.path(output_zarr, metadata_file))
    expect_equal(md[["zarr_format"]], zarr_version)
    expect_true(file.exists(file.path(output_zarr, "group1", metadata_file)))

    # create nested two groups
    create_zarr_group(zarr_path = output_zarr, group = "group2/subgroup1")
    expect_true(dir.exists(file.path(output_zarr, "group2")))
    expect_true(file.exists(file.path(output_zarr, "group2", metadata_file)))
    expect_true(dir.exists(file.path(output_zarr, "group2/subgroup1")))
    expect_true(file.exists(file.path(
      output_zarr,
      "group2/subgroup1",
      metadata_file
    )))

    # create nested three groups
    create_zarr_group(
      zarr_path = output_zarr,
      group = "group3/subgroup1/subsubgroup1"
    )
    expect_true(dir.exists(file.path(output_zarr, "group3")))
    expect_true(file.exists(file.path(output_zarr, "group3", metadata_file)))
    expect_true(dir.exists(file.path(output_zarr, "group3/subgroup1")))
    expect_true(file.exists(file.path(
      output_zarr,
      "group3/subgroup1",
      metadata_file
    )))
    expect_true(dir.exists(file.path(
      output_zarr,
      "group3/subgroup1/subsubgroup1"
    )))
    expect_true(file.exists(file.path(
      output_zarr,
      "group3/subgroup1/subsubgroup1",
      metadata_file
    )))
  })
}

test_that("create_zarr rejects unsupported zarr_version", {
  output_zarr <- tempfile(fileext = ".zarr")
  expect_error(
    create_zarr(zarr_path = output_zarr, zarr_version = "v4"),
    "Incorrect Zarr version specified"
  )
})
