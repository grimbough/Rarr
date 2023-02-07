
path <- tempfile()
res <- create_empty_zarr_array(zarr_array_path = path, 
                               dim = 100, chunk_dim = 10, data_type = "integer", 
                               fill_value = 100L)

x <- array(1:5, dim = 5)
expect_true(update_zarr_array(path, x = x, index = list(1:5)))
expect_identical(read_zarr_array(path), array(c(1:5, rep(100L, 95)), dim = 100))
## only a single chunk file should have been created
expect_identical( list.files(path), "0")


x <- rep(20L, 5);
expect_true(update_zarr_array(path, x = x, index = list( c(91, 93, 95, 97, 99)  )))
expect_identical(read_zarr_array(path)[91:100], array(rep(c(20L, 100L), 5), dim = 10))
## only a single chunk file should have been created
expect_true( length(list.files(path)) == 2 )
