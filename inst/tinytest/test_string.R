zarr_c <- system.file("extdata", "zarr_examples", "column-first", "string.zarr",
                      package = "Rarr")
zarr_f <- system.file("extdata", "zarr_examples", "row-first", "string.zarr",
                      package = "Rarr")
index <- list(1:5, 1:10, 1)


## Shouldn't be any warnings for this input
expect_silent( column_major <- read_zarr_array(zarr_c, index = index) )
expect_silent( row_major <- read_zarr_array(zarr_f, index = index) )

## row and column major should be read the same in R
expect_identical(column_major, row_major)

## we return an array
expect_inherits(column_major, "array")

## dimensions equal to the index
expect_equal(dim(column_major), sapply(index, length))

## first row should be all "test" except the first element which is "ready"
expect_equal(column_major[1,,], c("ready", rep("test", length(index[[2]])-1)))
## first column should be all "ready"
expect_true(all(column_major[,1,] == "ready"))


## check read/write produce the same things
path <- tempfile()
expect_silent(
  res <- write_zarr_array(
    x = column_major, 
    zarr_array_path = path,
    chunk_dim = c(2, 5, 1)
  )
)
expect_identical(read_zarr_array(path), column_major)

## check we truncate strings if the exceed the specified length
path <- tempfile()
expect_silent(
  res <- write_zarr_array(
    x = column_major, 
    zarr_array_path = path,
    chunk_dim = c(2, 5, 1),
    nchar = 1
  )
)
expect_equal(max(nchar(read_zarr_array(path))), 1L)

########################

greetings <- c('¡Hola mundo!', 'Hej Världen!', 'Servus Woid!', 'Hei maailma!',
               'Xin chào thế giới', 'Njatjeta Botë!', 'Γεια σου κόσμε!',
               'こんにちは世界', '世界，你好！', 'Helló, világ!', 'Zdravo svete!',
               'เฮลโลเวิลด์')

unicode_fixed_length <- system.file("extdata", "zarr_examples", "column-first", "Unicode.zarr",
                      package = "Rarr")

expect_silent(
  res <- read_zarr_array(unicode_fixed_length)
)
expect_equal(res[,1], greetings)
expect_equal(res[1,], greetings)

unicode_var_length <- system.file("extdata", "zarr_examples", "column-first", "Unicode.zarr",
                                    package = "Rarr")

expect_silent(
  res <- read_zarr_array(unicode_var_length)
)
expect_equal(res[,1], greetings)
expect_equal(res[1,], greetings)


## writing & reading unicode
path <- tempfile()
create_empty_zarr_array(path, dim = c(12,12), chunk_dim = c(6,6), 
                        data_type = "<U", nchar = 20)
x <- array("", dim = c(12,12))
x[1,] <- greetings
x[,1] <- greetings
update_zarr_array(zarr_array_path = path, x, index = list(1:12, 1:12))
expect_identical(read_zarr_array(path), x)
