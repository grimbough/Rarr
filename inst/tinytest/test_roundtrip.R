path <- file.path(tempdir(), "empty_100.zarr")
expect_silent(
    res <- create_empty_zarr_array(
        zarr_array_path = path,
        dim = 100, chunk_dim = 10,
        data_type = "integer", fill_value = 100L
    )
)
expect_true(res)
expect_identical(read_zarr_array(path), array(100L, dim = c(100)))


x <- array(runif(n = 1000, min = -10, max = 10), dim = c(10, 20, 5))

## Standard writing and reading
path <- tempfile(pattern = "zlib_")
expect_silent(res <- write_zarr_array(
    x = x, zarr_array_path = path,
    chunk_dim = c(2, 5, 1), order = "F",
    compressor = use_zlib()
))
expect_identical(read_zarr_array(path), x)

## Standard writing and reading
path <- tempfile(pattern = "gzip_")
expect_silent(res <- write_zarr_array(
  x = x, zarr_array_path = path,
  chunk_dim = c(2, 5, 1), order = "F",
  compressor = use_gzip()
))
expect_identical(read_zarr_array(path), x)

## testing blosc compression
path <- tempfile(pattern = "blosc_")
expect_silent(res <- write_zarr_array(
    x = x, zarr_array_path = path,
    chunk_dim = c(2, 5, 1),
    compressor = Rarr:::use_blosc()
))
expect_identical(read_zarr_array(path), x)

## testing LZMA compression
path <- tempfile(pattern = "lzma_")
expect_silent(res <- write_zarr_array(
    x = x, zarr_array_path = path,
    chunk_dim = c(2, 5, 1),
    compressor = Rarr:::use_lzma()
))
expect_identical(read_zarr_array(path), x)

## testing LZ4 compression
path <- tempfile(pattern = "lz4_")
expect_silent(res <- write_zarr_array(
  x = x, zarr_array_path = path,
  chunk_dim = c(2, 5, 1),
  compressor = Rarr:::use_lz4()
))
expect_identical(read_zarr_array(path), x)

## testing BZIP2 compression
path <- tempfile(pattern = "bz2_")
expect_silent(res <- write_zarr_array(
  x = x, zarr_array_path = path,
  chunk_dim = c(2, 5, 1),
  compressor = Rarr:::use_bz2()
))
expect_identical(read_zarr_array(path), x)

## testing no compression
path <- tempfile()
expect_silent(res <- write_zarr_array(
  x = x, zarr_array_path = path,
  chunk_dim = c(2, 5, 1),
  compressor = NULL
))
expect_identical(read_zarr_array(path), x)

## testing chunk dimensions that don't align perfectly with the array extent
path <- tempfile()
expect_silent(res <- write_zarr_array(
    x = x, zarr_array_path = path,
    chunk_dim = c(6, 11, 3)
))
expect_identical(read_zarr_array(path), x)

## row-major ordering
path <- tempfile()
expect_silent(
    res <- write_zarr_array(
        x = x, zarr_array_path = path,
        chunk_dim = c(2, 5, 1), order = "C"
    )
)
expect_identical(read_zarr_array(path), x)

