test_that("empty zarr arrays can be created and read correctly", {
  path <- withr::local_tempdir(fileext = ".zarr")

  expect_silent(
    res <- create_empty_zarr_array(
      zarr_array_path = path,
      dim = 100,
      chunk_dim = 10,
      data_type = "integer",
      fill_value = 100L
    )
  )

  expect_identical(read_zarr_array(path), array(100L, dim = 100))
})

test_that("zarr arrays can be written and read with various compression methods", {
  x <- array(runif(n = 1000, min = -10, max = 10), dim = c(10, 20, 5))

  # Standard writing and reading with zlib compression
  path <- withr::local_tempfile(pattern = "zlib_", fileext = ".zarr")
  expect_silent(
    res <- write_zarr_array(
      x = x,
      zarr_array_path = path,
      chunk_dim = c(2, 5, 1),
      order = "F",
      compressor = use_zlib()
    )
  )
  expect_identical(read_zarr_array(path), x)

  # Standard writing and reading with gzip compression
  path <- withr::local_tempfile(pattern = "gzip_", fileext = ".zarr")
  expect_silent(
    res <- write_zarr_array(
      x = x,
      zarr_array_path = path,
      chunk_dim = c(2, 5, 1),
      order = "F",
      compressor = use_gzip()
    )
  )
  expect_identical(read_zarr_array(path), x)

  # Testing blosc compression
  path <- withr::local_tempfile(pattern = "blosc_", fileext = ".zarr")
  expect_silent(
    res <- write_zarr_array(
      x = x,
      zarr_array_path = path,
      chunk_dim = c(2, 5, 1),
      compressor = use_blosc(
        cname = "blosclz",
        clevel = 9L,
        shuffle = "bitshuffle",
        typesize = 8L,
        blocksize = 0L
      )
    )
  )
  expect_identical(read_zarr_array(path), x)

  # Testing LZMA compression
  path <- withr::local_tempfile(pattern = "lzma_", fileext = ".zarr")
  expect_silent(
    res <- write_zarr_array(
      x = x,
      zarr_array_path = path,
      chunk_dim = c(2, 5, 1),
      compressor = use_lzma()
    )
  )
  expect_identical(read_zarr_array(path), x)

  # Testing LZ4 compression
  path <- withr::local_tempfile(pattern = "lz4_", fileext = ".zarr")
  expect_silent(
    res <- write_zarr_array(
      x = x,
      zarr_array_path = path,
      chunk_dim = c(2, 5, 1),
      compressor = use_lz4()
    )
  )
  expect_identical(read_zarr_array(path), x)

  # Testing BZIP2 compression
  path <- withr::local_tempfile(pattern = "bz2_", fileext = ".zarr")
  expect_silent(
    res <- write_zarr_array(
      x = x,
      zarr_array_path = path,
      chunk_dim = c(2, 5, 1),
      compressor = use_bz2()
    )
  )
  expect_identical(read_zarr_array(path), x)

  # Testing ZSTD compression
  path <- withr::local_tempfile(pattern = "zstd_", fileext = ".zarr")
  expect_silent(
    res <- write_zarr_array(
      x = x,
      zarr_array_path = path,
      chunk_dim = c(2, 5, 1),
      compressor = use_zstd()
    )
  )
  expect_identical(read_zarr_array(path), x)

  # Testing no compression
  path <- withr::local_tempfile(fileext = ".zarr")
  expect_silent(
    res <- write_zarr_array(
      x = x,
      zarr_array_path = path,
      chunk_dim = c(2, 5, 1),
      compressor = NULL
    )
  )
  expect_identical(read_zarr_array(path), x)
})

test_that("zarr arrays work with non-aligned chunk dimensions", {
  x <- array(runif(n = 1000, min = -10, max = 10), dim = c(10, 20, 5))

  # Testing chunk dimensions that don't align perfectly with the array extent
  path <- withr::local_tempfile(fileext = ".zarr")
  expect_silent(
    res <- write_zarr_array(
      x = x,
      zarr_array_path = path,
      chunk_dim = c(6, 11, 3)
    )
  )
  expect_identical(read_zarr_array(path), x)
})

test_that("zarr arrays work with row-major ordering", {
  x2d <- array(1:8, dim = c(2, 4))
  path2d <- withr::local_tempfile(fileext = ".zarr")
  expect_silent(
    write_zarr_array(
      x = x2d,
      zarr_array_path = path2d,
      chunk_dim = c(1, 2),
      order = "C"
    )
  )
  expect_identical(read_zarr_array(path2d), x2d)

  x3d <- array(seq_len(300), dim = c(6, 10, 5))

  path3d <- withr::local_tempfile(fileext = ".zarr")
  expect_silent(
    write_zarr_array(
      x = x3d,
      zarr_array_path = path3d,
      chunk_dim = c(2, 5, 1),
      order = "C"
    )
  )
  expect_identical(read_zarr_array(path3d), x3d)
})

test_that("dimension names roundtrip", {
  x <- array(1:32, dim = c(2, 4, 4))
  dimnames(x) <- list("X" = NULL, "Y" = NULL, "Z" = NULL)

  path <- withr::local_tempfile(fileext = ".zarr")

  expect_silent(
    write_zarr_array(
      x = x,
      zarr_array_path = path,
      chunk_dim = c(2, 4, 1)
    )
  )
  res <- read_zarr_array(path)
  expect_identical(res, x)
})

test_that("NAs roundtrip", {
  int_nas <- array(c(1L, NA_integer_, 3L))
  path_int <- withr::local_tempfile(fileext = ".zarr")
  expect_silent(
    write_zarr_array(
      x = int_nas,
      zarr_array_path = path_int,
      chunk_dim = 3
    )
  )

  expect_identical(
    read_zarr_array(path_int),
    int_nas
  )

  dbl_nas <- array(c(1.5, NA_real_, 3.5))
  path_dbl <- withr::local_tempfile(fileext = ".zarr")
  expect_silent(
    write_zarr_array(
      x = dbl_nas,
      zarr_array_path = path_dbl,
      chunk_dim = 3
    )
  )

  expect_identical(
    read_zarr_array(path_dbl),
    dbl_nas
  )

  char_nas <- array(c("z", NA_character_, "WORD"))
  path_unicode <- withr::local_tempfile(fileext = ".zarr")
  expect_silent(
    write_zarr_array(
      x = char_nas,
      zarr_array_path = path_unicode,
      chunk_dim = 3,
      data_type = "<U"
    )
  )

  expect_identical(
    read_zarr_array(path_unicode),
    char_nas
  )

  path_char <- withr::local_tempfile(fileext = ".zarr")
  expect_silent(
    write_zarr_array(
      x = char_nas,
      zarr_array_path = path_char,
      chunk_dim = 3,
      data_type = "|S"
    )
  )

  expect_identical(
    read_zarr_array(path_char),
    char_nas
  )
})
