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
write_zattrs(path = path, new.zattrs = zattrs)
expect_true(file.exists(file.path(path, ".zattrs")))

# check .zattrs
read.zattrs <- read_zattrs(path)
expect_equal(read.zattrs, zattrs)

# add new elements to .zattrs
zattrs.new.elem <- list(foo2 = "foo")
write_zattrs(path = path, new.zattrs = zattrs.new.elem)
read.zattrs <- read_zattrs(path)
expect_equal(read.zattrs, c(zattrs, zattrs.new.elem))

# overwrite
zattrs.new.elem <- list(foo2 = "foo2")
write_zattrs(path = path, new.zattrs = zattrs.new.elem)
read.zattrs <- read_zattrs(path)
zattrs[names(zattrs.new.elem)] <- zattrs.new.elem
expect_equal(read.zattrs, c(zattrs))

# overwrite = FALSE
# TODO: should we control for the order of elements when overwritten
zattrs.new.elem <- list(foo2 = "foo")
write_zattrs(path = path, new.zattrs = zattrs.new.elem, overwrite = FALSE)
read.zattrs <- read_zattrs(path)
zattrs[names(zattrs.new.elem)] <- "foo2"
expect_true(all(names(read.zattrs) %in% names(zattrs)))
expect_true(all(read.zattrs %in% zattrs))

# test lists with empty names
zattrs.new.elem <- list("empty", full = "full")
expect_message(write_zattrs(path = path, new.zattrs = zattrs.new.elem))
read.zattrs <- read_zattrs(path)
zattrs[["full"]] <- "full"
expect_true(all(names(read.zattrs) %in% names(zattrs)))
expect_true(all(read.zattrs %in% zattrs))
