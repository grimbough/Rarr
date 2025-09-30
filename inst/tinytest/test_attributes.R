# create zarr array
dir.create(td <- tempfile())
path <- file.path(td, "test.zarr")
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
expect_equal(read.zattrs, zattrs)

# add new elements to .zattrs
zattrs.new.elem <- list(foo2 = "foo")
write_zarr_attributes(zarr_path = path, new.zattrs = zattrs.new.elem)
read.zattrs <- read_zarr_attributes(path)
expect_equal(read.zattrs, c(zattrs, zattrs.new.elem))

# overwrite
zattrs.new.elem <- list(foo2 = "foo2")
write_zarr_attributes(zarr_path = path, new.zattrs = zattrs.new.elem)
read.zattrs <- read_zarr_attributes(path)
zattrs[names(zattrs.new.elem)] <- zattrs.new.elem
expect_equal(read.zattrs, c(zattrs))

# overwrite = FALSE
# TODO: should we control for the order of elements when overwritten
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

# v3
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
