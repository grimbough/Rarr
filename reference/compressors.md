# Define compression tool and settings

These functions select a compression tool and its setting when writing a
Zarr file

## Usage

``` r
use_blosc(cname = "lz4")

use_zlib(level = 6L)

use_gzip(level = 6L)

use_bz2(level = 6L)

use_lzma(level = 9L)

use_lz4()

use_zstd(level = 3)
```

## Arguments

- cname:

  Blosc is a 'meta-compressor' providing access to several compression
  algorithms. This argument defines which compression tool should be
  used. Valid options are: 'lz4', 'lz4hc', 'blosclz', 'zstd', 'zlib',
  'snappy'.

- level:

  Specify the compression level to use. The range of possible values is
  dependant on the compression tool being used. For example, for
  `use_zlib()` this argument can be between 1 & 9, while for
  `use_zstd()`the valid range is 1 to 22.

## Value

A list containing the details of the selected compression tool. This
will be written to the .zarray metadata when the Zarr array is created.

## Examples

``` r
## define 2 compression filters for blosc (using snappy) and bzip2 (level 5)
blosc_with_snappy_compression <- use_blosc(cname = "snappy")
bzip2_compression <- use_bz2(level = 5)

## create an example array to write to a file
x <- array(runif(n = 1000, min = -10, max = 10), dim = c(10, 20, 5))

## write the array to two files using each compression filter
blosc_path <- tempfile()
bzip2_path <- tempfile()
write_zarr_array(
  x = x, zarr_array_path = blosc_path, chunk_dim = c(2, 5, 1),
  compressor = blosc_with_snappy_compression
)
write_zarr_array(
  x = x, zarr_array_path = bzip2_path, chunk_dim = c(2, 5, 1),
  compressor = bzip2_compression
)

## the contents of the two arrays should be the same
identical(read_zarr_array(blosc_path), read_zarr_array(bzip2_path))
#> [1] TRUE

## the size of the files on disk are not the same
sum(file.size(list.files(blosc_path, full.names = TRUE)))
#> [1] 9600
sum(file.size(list.files(bzip2_path, full.names = TRUE)))
#> [1] 13133
```
