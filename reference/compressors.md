# Define compression tool and settings

These functions select a compression tool and its setting when writing a
Zarr file

## Usage

``` r
use_blosc(
  cname = c("lz4", "lz4hc", "blosclz", "zstd", "zlib", "snappy"),
  clevel = 5L,
  shuffle = c("shuffle", "noshuffle", "bitshuffle"),
  typesize = NULL,
  blocksize = 0L
)

use_zlib(level = 6L)

use_gzip(level = 6L)

use_bz2(level = 6L)

use_lzma(level = 9L)

use_lz4()

use_zstd(level = 0L)
```

## Arguments

- cname:

  Blosc is a 'meta-compressor' providing access to several compression
  algorithms. This argument defines which compression tool should be
  used. Valid options are: `"lz4"`, `"lz4hc"`, `"blosclz"`, `"zstd"`,
  `"zlib"`, `"snappy"`.

- clevel:

  An integer from 0 to 9 which controls the speed and level of
  compression. A level of 1 is the fastest compression method and
  produces the least compressions, while 9 is slowest and produces the
  most compression. Compression is turned off completely when level
  is 0. Defaults to 5.

- shuffle:

  Specifies the type of shuffling to perform, if any, prior to
  compression. Must be one of `"noshuffle"`, to indicate no shuffling;
  `"shuffle"` (default), to indicate byte-wise shuffling;
  `"bitshuffle"`, to indicate bit-wise shuffling.

- typesize:

  The data type size in bytes used by Blosc shuffling. If `NULL`
  (default), this will be inferred from the array datatype. Ignored if
  `shuffle = "noshuffle"`.

- blocksize:

  The requested size of the compressed blocks in bytes. Use 0 (default)
  to let Blosc choose automatically.

- level:

  Specify the compression level to use. The range of possible values is
  dependant on the compression tool being used. For example, for
  `use_zlib()` this argument can be between 1 & 9, while for
  `use_zstd()`the valid range is 0 (default; level chosen automatically)
  to 22.

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
#> [1] 4879
sum(file.size(list.files(bzip2_path, full.names = TRUE)))
#> [1] 4772
```
