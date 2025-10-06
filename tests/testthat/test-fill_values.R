test_that("fill values work correctly for zarr arrays", {
  zarr_arrays <- list.files(
    system.file("extdata", "zarr_examples", "fill-values", package = "Rarr"),
    full.names = TRUE
  )

  for (zarr in zarr_arrays) {
    if (grepl("-inf.zarr", zarr)) {
      fill_val <- Inf
    } else if (grepl("-neginf.zarr", zarr)) {
      fill_val <- -Inf
    }

    # Shouldn't be any warnings for this input
    expect_silent(data <- read_zarr_array(zarr, index = list(1:2, 1:10)))

    # The first row should all be 1
    expect_identical(data[1, ], rep(1, 10))

    # Second row is Inf or -Inf
    expect_true(all(data[2, ] == fill_val))

    # Now we read a chunk that isn't on disk, only generated from fill value
    expect_silent(data <- read_zarr_array(zarr, index = list(19:20, 1:10)))

    # Everything should be fill value
    expect_true(all(data == fill_val))
  }
})
