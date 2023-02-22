zarr_c <- system.file("extdata", "zarr_examples", "column-first", "boolean.zarr",
                      package = "Rarr")
index <- list(1:5, NULL)

## Shouldn't be any warnings for this input
expect_silent( column_major <- read_zarr_array(zarr_c, index = index) )

## we return an array
expect_inherits(column_major, "array")

## dimensions equal to the index
expect_equal(dim(column_major), c(5,10))

## first row should be TRUE
expect_true(all(column_major[1,]))
expect_true(!any(column_major[2,]))

