
path <- file.path(tempdir(), "empty_100.zarr")
expect_silent(res <- create_empty_zarr_array(zarr_array_path = path, 
                                             dim = 100, chunk_dim = 10, data_type = "integer", fill_value = 100L))
expect_true(res)
expect_identical(read_zarr_array(path), array(100L, dim = c(100)))


x <- array(runif(n = 1000, min = -10, max = 10), dim = c(10, 20, 5))
path <- file.path(tempdir(), "-10_10.zarr")
expect_silent(res <- write_zarr_array(x = x, zarr_array_path = path, chunk_dim = c(2, 5, 1)))
expect_identical(read_zarr_array(path), x)

## testing blosc compression
path <- tempfile()
expect_silent(res <- write_zarr_array(x = x, zarr_array_path = path, 
                                      chunk_dim = c(2, 5, 1), 
                                      compressor = use_blosc()))
expect_identical(read_zarr_array(path), x)
