zarr <- system.file("extdata", "zarr_examples", "fill-values", "double-inf.zarr",
                      package = "Rarr")
zarr_arrays <- list.files(system.file("extdata", "zarr_examples", "fill-values", package = "Rarr"), full.names = TRUE)

for(zarr in zarr_arrays) {
  
  if(grepl("-inf.zarr", zarr)) { fill_val <- Inf } else if (grepl("-neginf.zarr", zarr)) { fill_val <- -Inf }

  ## Shouldn't be any warnings for this input
  expect_silent( data <- read_zarr_array(zarr, index = list(1:2, 1:10)) )

  ## the first row should all be 1
  expect_identical(data[1,], rep(1, 10))

  ## second row is Inf or -Inf
  expect_true(all(data[2,] == fill_val))

  ## Now we read a chunk that isn't on disk, only generated from fill value
  expect_silent( data <- read_zarr_array(zarr, index = list(19:20, 1:10)) )
  
  ## everything should be fill value
  expect_true(all(data == fill_val))
}
