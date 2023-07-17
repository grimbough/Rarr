zarr_c <- system.file("extdata", "zarr_examples", "column-first", "int64.zarr",
                      package = "Rarr")

## return results as a data.frame
df <- zarr_overview(zarr_c, as_data_frame = TRUE)
expect_inherits(df, "data.frame")
expect_equal(dim(df), c(1,6))

## write details to screen
expect_stdout(zarr_overview(zarr_c, as_data_frame = FALSE))
