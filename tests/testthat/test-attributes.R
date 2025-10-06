test_that("zarr attributes can be written and read", {
  # create zarr array
  path <- withr::local_tempdir(fileext = ".zarr")
  x <- array(runif(n = 10), dim = c(2, 5))
  res <- write_zarr_array(
    x = x,
    zarr_array_path = path,
    chunk_dim = c(2, 5)
  )

  # add .zattrs to /
  zattrs <- list(foo = "foo", bar = "bar")
  write_zarr_attributes(zarr_path = path, new.zattrs = zattrs)
  expect_true(file.exists(file.path(path, ".zattrs")))

  # check .zattrs
  read.zattrs <- read_zarr_attributes(path)
  expect_identical(read.zattrs, zattrs)
})

test_that("zarr attributes can be updated with new elements", {
  # create zarr array
  path <- withr::local_tempdir(fileext = ".zarr")
  x <- array(runif(n = 10), dim = c(2, 5))
  res <- write_zarr_array(
    x = x,
    zarr_array_path = path,
    chunk_dim = c(2, 5)
  )

  zattrs <- list(foo = "foo", bar = "bar")
  write_zarr_attributes(zarr_path = path, new.zattrs = zattrs)

  # add new elements to .zattrs
  zattrs.new.elem <- list(foo2 = "foo")
  write_zarr_attributes(zarr_path = path, new.zattrs = zattrs.new.elem)
  read.zattrs <- read_zarr_attributes(path)
  expect_identical(read.zattrs, c(zattrs, zattrs.new.elem))
})

test_that("zarr attributes can be overwritten", {
  # create zarr array
  path <- withr::local_tempdir(fileext = ".zarr")
  x <- array(runif(n = 10), dim = c(2, 5))
  res <- write_zarr_array(
    x = x,
    zarr_array_path = path,
    chunk_dim = c(2, 5)
  )

  zattrs <- list(foo = "foo", bar = "bar")
  write_zarr_attributes(zarr_path = path, new.zattrs = zattrs)

  # overwrite
  zattrs.new.elem <- list(foo2 = "foo2")
  write_zarr_attributes(zarr_path = path, new.zattrs = zattrs.new.elem)
  read.zattrs <- read_zarr_attributes(path)
  zattrs[names(zattrs.new.elem)] <- zattrs.new.elem
  expect_identical(read.zattrs, c(zattrs))
})

test_that("zarr attributes overwrite parameter works correctly", {
  # create zarr array
  path <- withr::local_tempdir(fileext = ".zarr")
  x <- array(runif(n = 10), dim = c(2, 5))
  res <- write_zarr_array(
    x = x,
    zarr_array_path = path,
    chunk_dim = c(2, 5)
  )

  zattrs <- list(foo = "foo", bar = "bar", foo2 = "foo2")
  write_zarr_attributes(zarr_path = path, new.zattrs = zattrs)

  # overwrite = FALSE
  zattrs.new.elem <- list(foo2 = "foo")
  write_zarr_attributes(
    zarr_path = path,
    new.zattrs = zattrs.new.elem,
    overwrite = FALSE
  )
  read.zattrs <- read_zarr_attributes(path)
  zattrs[names(zattrs.new.elem)] <- "foo2"
  expect_true(all(names(read.zattrs) %in% names(zattrs)))
  expect_true(all(read.zattrs %in% zattrs))
})

test_that("zarr attributes handles lists with empty names", {
  # create zarr array
  path <- withr::local_tempdir(fileext = ".zarr")
  x <- array(runif(n = 10), dim = c(2, 5))
  res <- write_zarr_array(
    x = x,
    zarr_array_path = path,
    chunk_dim = c(2, 5)
  )

  zattrs <- list(foo = "foo", bar = "bar")
  write_zarr_attributes(zarr_path = path, new.zattrs = zattrs)

  # test lists with empty names
  zattrs.new.elem <- list("empty", full = "full")
  expect_message(write_zarr_attributes(
    zarr_path = path,
    new.zattrs = zattrs.new.elem
  ))
  read.zattrs <- read_zarr_attributes(path)
  zattrs[["full"]] <- "full"
  expect_true(all(names(read.zattrs) %in% names(zattrs)))
  expect_true(all(read.zattrs %in% zattrs))
})

test_that("v3 zarr attributes work correctly", {
  v3_attrs <- read_zarr_attributes(
    system.file(
      "extdata",
      "zarr_examples",
      "metadata",
      "v3_attr.zarr",
      package = "Rarr"
    )
  )
  expect_identical(
    v3_attrs,
    list(custom = "Hello, Zarr!")
  )

  expect_identical(
    read_zarr_attributes(
      system.file(
        "extdata",
        "zarr_examples",
        "metadata",
        "v3.zarr",
        package = "Rarr"
      )
    ),
    list()
  )
})
