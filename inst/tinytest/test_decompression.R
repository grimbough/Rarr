zarr_arrays <- list.files(system.file("extdata", "zarr_examples", "compression", 
                                      package = "Rarr"), full.names = TRUE)

for(zarr in zarr_arrays) {
  
  ## Shouldn't be any warnings for this input
  expect_silent( data <- read_zarr_array(zarr) )

  ## the first row should all be 1 to 10
  expect_identical(data[1,], 1:10)

  ## first column is sequence increasing by 3
  expect_true(all(data[,1] == seq(1,60,3)))
}
