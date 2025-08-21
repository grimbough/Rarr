m <- matrix(runif(1e5), ncol = 100)
tf1 <- tempfile(fileext = ".zarr")
chunk_dim <- c(50, 50)
z1 <- writeZarrArray(m, tf1, chunk_dim = chunk_dim)

expect_inherits(z1, class = "ZarrArray")

# getters
expect_equal(chunkdim(z1), chunk_dim)
## input and output path are not the same on all platforms e.g. Windows
## as we follow the Zarr spec rules for normalizing
expect_equal(path(z1), Rarr:::.normalize_array_path(tf1))

# ways to get the array from disk to memory
expect_equal(extract_array(z1, index = list(NULL, NULL)), m)
expect_equal(realize(z1, BACKEND = NULL), m)

# write and read ZarrArray with dim being non divisible by chunk_dim
# should return as usual when dim = 1000,100 and chunk_dim = 500,60
tf2 <- tempfile(fileext = ".zarr")
tmp <- writeZarrArray(x = m, zarr_array_path = tf2, chunk_dim = c(500, 60))
expect_equal(m, realize(tmp))

# create ZarrArray with character data type
m <- matrix("array", ncol = 10, nrow = 10)
tf2 <- tempfile(fileext = ".zarr")
chunk_dim <- c(10, 10)
z2 <- writeZarrArray(m, tf2, chunk_dim = chunk_dim)

expect_inherits(z2, class = "ZarrArray")

expect_equal(type(z2), "character")
