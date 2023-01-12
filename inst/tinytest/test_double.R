zarr_c <- system.file("extdata", "zarr_examples", "column-first", "double.zarr",
                      package = "Rarr")
zarr_f <- system.file("extdata", "zarr_examples", "row-first", "double.zarr",
                      package = "Rarr")
index <- list(1:30, 1:20, 1)


## Shouldn't be any warnings for this input
expect_silent( column_major <- read_zarr_array(zarr_c, index = index) )
expect_silent( row_major <- read_zarr_array(zarr_f, index = index) )

## row and column major should be read the same in R
expect_identical(column_major, row_major)

## we return an array
expect_inherits(column_major, "array")

## dimensions equal to the index
expect_equal(dim(column_major), sapply(index, length))

## first row should be all "test" except the first element which is "ready"
expect_equal(column_major[1,,], c("ready", rep("test", length(index[[2]])-1)))
## first column should be all "ready"
expect_true(all(column_major[,1,] == "ready"))
