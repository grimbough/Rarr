test_that("string zarr arrays can be read correctly", {
  zarr_c <- system.file(
    "extdata",
    "zarr_examples",
    "column-first",
    "string.zarr",
    package = "Rarr"
  )
  zarr_f <- system.file(
    "extdata",
    "zarr_examples",
    "row-first",
    "string.zarr",
    package = "Rarr"
  )
  index <- list(1:5, 1:10, 1)

  ## Shouldn't be any warnings for this input
  expect_silent(column_major <- read_zarr_array(zarr_c, index = index))
  expect_silent(row_major <- read_zarr_array(zarr_f, index = index))

  ## row and column major should be read the same in R
  expect_identical(column_major, row_major)

  ## we return an array
  expect_true(is.array(column_major))

  ## dimensions equal to the index
  expect_identical(dim(column_major), lengths(index))

  ## first row should be all "test" except the first element which is "ready"
  expect_identical(
    column_major[1, , ],
    c("ready", rep("test", length(index[[2]]) - 1))
  )
  ## first column should be all "ready"
  expect_true(all(column_major[, 1, ] == "ready"))

  ## check read/write produce the same things
  path <- withr::local_tempfile(fileext = ".zarr")
  expect_silent(
    res <- write_zarr_array(
      x = column_major,
      zarr_array_path = path,
      chunk_dim = c(2, 5, 1)
    )
  )
  expect_identical(read_zarr_array(path), column_major)

  ## check we truncate strings if the exceed the specified length
  path <- withr::local_tempfile(fileext = ".zarr")
  expect_silent(
    res <- write_zarr_array(
      x = column_major,
      zarr_array_path = path,
      chunk_dim = c(2, 5, 1),
      nchar = 1
    )
  )
  expect_identical(max(nchar(read_zarr_array(path))), 1L)

  ## check if nchar argument is provided when creating empty character zarr arrays
  path <- withr::local_tempfile(fileext = ".zarr")
  expect_error(
    create_empty_zarr_array(
      zarr_array_path = path,
      dim = dim(column_major),
      chunk_dim = c(2, 5, 1),
      data_type = storage.mode(column_major)
    )
  )

  ########################

  greetings <- c(
    '¡Hola mundo!',
    'Hej Världen!',
    'Servus Woid!',
    'Hei maailma!',
    'Xin chào thế giới',
    'Njatjeta Botë!',
    'Γεια σου κόσμε!',
    'こんにちは世界',
    '世界，你好！',
    'Helló, világ!',
    'Zdravo svete!',
    'เฮลโลเวิลด์'
  )

  unicode_fixed_length <- system.file(
    "extdata",
    "zarr_examples",
    "column-first",
    "Unicode.zarr",
    package = "Rarr"
  )

  expect_silent(
    res <- read_zarr_array(unicode_fixed_length)
  )
  expect_identical(res[, 1], greetings)
  expect_identical(res[1, ], greetings)

  unicode_var_length <- system.file(
    "extdata",
    "zarr_examples",
    "column-first",
    "vlenUTF8.zarr",
    package = "Rarr"
  )

  expect_silent(
    res <- read_zarr_array(unicode_var_length)
  )
  expect_identical(res[, 1], greetings)
  expect_identical(res[1, ], greetings)

  ## writing & reading unicode
  path <- withr::local_tempfile(fileext = ".zarr")
  create_empty_zarr_array(
    path,
    dim = c(12, 12),
    chunk_dim = c(6, 6),
    data_type = "<U",
    nchar = 20
  )
  x <- array("", dim = c(12, 12))
  x[1, ] <- greetings
  x[, 1] <- greetings
  update_zarr_array(zarr_array_path = path, x, index = list(1:12, 1:12))
  expect_identical(read_zarr_array(path), x)
})

test_that("v2 and v3 return identical results", {
  zarr_v2 <- system.file(
    "extdata",
    "zarr_examples",
    "column-first",
    "string.zarr",
    package = "Rarr"
  )
  zarr_v3 <- system.file(
    "extdata",
    "zarr_examples",
    "column-first",
    "string_v3.zarr",
    package = "Rarr"
  )

  expect_no_condition(s_v2 <- read_zarr_array(zarr_v2))
  expect_error(
    read_zarr_array(zarr_v3),
    "Only base data types (not extensions) are supported for Zarr v3 arrays for now",
    fixed = TRUE
  )

  zarr_vlen_utf8_v2 <- system.file(
    "extdata",
    "zarr_examples",
    "column-first",
    "vlenUTF8.zarr",
    package = "Rarr"
  )
  zarr_vlen_utf8_v3 <- system.file(
    "extdata",
    "zarr_examples",
    "column-first",
    "vlenUTF8_v3.zarr",
    package = "Rarr"
  )
  expect_no_condition(vlen_utf8_v2 <- read_zarr_array(zarr_vlen_utf8_v2))
  expect_no_condition(vlen_utf8_v3 <- read_zarr_array(zarr_vlen_utf8_v3))

  expect_identical(vlen_utf8_v2, vlen_utf8_v3)
})

test_that("fill_value is converted to string", {
  # https://github.com/Huber-group-EMBL/Rarr/issues/94
  path <- withr::local_tempfile(fileext = ".zarr")
  expect_silent(
    create_empty_zarr_array(
      zarr_array_path = path,
      dim = c(4, 4),
      chunk_dim = c(2, 2),
      data_type = "character",
      fill_value = 0,
      nchar = 10
    )
  )
  expect_identical(read_zarr_array(path), matrix("0", nrow = 4, ncol = 4))
})
