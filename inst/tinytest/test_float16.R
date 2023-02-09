zarr_c <- system.file("extdata", "zarr_examples", "column-first", "float16.zarr",
                      package = "Rarr")
zarr_f <- system.file("extdata", "zarr_examples", "row-first", "float16.zarr",
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

## first row should be 2 to 20 except the first element which is -1
expect_equal(column_major[1,,], c(-1, 2:20))
## first column should be all 10.52
expect_equal(column_major[2:20,1,], rep(10.52, 19), tolerance = 0.0005)
expect_equal(column_major[2,2:4,], c(0.00005693, -5.97e-8, 0.0000039), tolerance = 0.0001)

expect_equal(column_major[3,2:4,], c(NaN, Inf, -Inf))
