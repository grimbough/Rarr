# Counting function calls ensures we don't repeatedly perform the same task when
# trying to read data (e.g., reading metadata over and over), which would be
# terrible for performance, but could happen more easily that one imagine in a
# complex codebase.

test_that("metadata is read and converted only once", {
  # Eventually, it would be great to use
  # https://github.com/r-lib/withr/issues/200
  local({
    read_metadata_calls <- 0L
    parsedatatype_calls <- 0L
    trace(
      Rarr:::.parse_datatype,
      function(x) {
        parsedatatype_calls <<- parsedatatype_calls + 1L
      },
      print = FALSE
    )
    trace(
      Rarr:::.read_array_metadata,
      function(x) {
        read_metadata_calls <<- read_metadata_calls + 1L
      },
      print = FALSE
    )
    read_zarr_array(
      system.file(
        "extdata",
        "zarr_examples",
        "column-first",
        "int32.zarr",
        package = "Rarr"
      )
    )
    expect_identical(read_metadata_calls, 1L)
    expect_identical(parsedatatype_calls, 1L)
  })

  local({
    convert_metadata_calls <- 0L
    trace(
      Rarr:::.convert_metadata_version,
      function(x) {
        convert_metadata_calls <<- convert_metadata_calls + 1L
      },
      print = FALSE
    )
    zarr_overview(
      system.file(
        "extdata",
        "zarr_examples",
        "column-first",
        "int32.zarr",
        package = "Rarr"
      )
    )
    expect_identical(convert_metadata_calls, 1L)
  })
})
