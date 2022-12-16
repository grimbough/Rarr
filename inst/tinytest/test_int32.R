zarr_c <- system.file("extdata", "zarr_examples", "column-first", "int32.zarr",
                      package = "Rarr")
zarr_f <- system.file("extdata", "zarr_examples", "row-first", "int32.zarr",
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
expect_equal(dim(column_major), c(30,20,1))

## first row should be sequence 1 to 20
expect_equal(column_major[1,,], 1:20)
## first column should be all 1s
expect_equal(column_major[,1,], rep(1,30))
