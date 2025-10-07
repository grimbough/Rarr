test_that("", {
  m <- matrix(runif(1e5), ncol = 100)
  tf1 <- withr::local_tempfile(fileext = ".zarr")

  chunk_dim <- c(50L, 50L)
  z1 <- writeZarrArray(m, tf1, chunk_dim = chunk_dim)

  expect_s4_class(z1, "ZarrArray")

  # getters
  expect_identical(chunkdim(z1), chunk_dim)
  ## input and output path are not the same on all platforms e.g. Windows
  ## as we follow the Zarr spec rules for normalizing
  expect_identical(path(z1), .normalize_array_path(tf1))

  # ways to get the array from disk to memory
  expect_identical(extract_array(z1, index = list(NULL, NULL)), m)
  expect_identical(realize(z1, BACKEND = NULL), m)

  # write and read ZarrArray with dim being non divisible by chunk_dim
  # should return as usual when dim = 1000,100 and chunk_dim = 500,60
  tf2 <- withr::local_tempfile(fileext = ".zarr")
  tmp <- writeZarrArray(x = m, zarr_array_path = tf2, chunk_dim = c(500, 60))
  expect_identical(m, realize(tmp))

  # create ZarrArray with character data type
  m <- matrix("array", ncol = 10, nrow = 10)
  tf3 <- withr::local_tempfile(fileext = ".zarr")
  chunk_dim <- c(10, 10)
  z2 <- writeZarrArray(m, tf3, chunk_dim = chunk_dim)

  expect_s4_class(z2, "ZarrArray")

  expect_identical(type(z2), "character")
})
