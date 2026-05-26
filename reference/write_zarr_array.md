# Write an R array to Zarr

Write an R array to Zarr

## Usage

``` r
write_zarr_array(
  x,
  zarr_array_path,
  chunk_dim,
  data_type = storage.mode(x),
  order = c("F", "C"),
  compressor = use_zstd(),
  fill_value = NULL,
  nchar,
  dimension_separator = if (zarr_version == 2L) "." else "/",
  zarr_version = 3L
)
```

## Arguments

- x:

  The R array that will be written to the Zarr array.

- zarr_array_path:

  Character vector of length 1 giving the path to the new Zarr array.

- chunk_dim:

  Dimensions of the array chunks. Should be a numeric vector with the
  same length as the `dim` argument.

- data_type:

  Character vector giving the data type of the new array. Valid options
  are: "integer", "double", "character", "logical", which are based on
  standard R data types. You can also use the analogous Numpy formats:
  "\|i1", "\<i2", "\<i4", "\<f4", "\<f8", "\|S", "\|b1". If this
  argument isn't provided the `fill_value` will be used to determine the
  datatype.

- order:

  Define the layout of the bytes within each chunk. Valid options are
  'column', 'row', 'F' & 'C'. 'column' or 'F' will specify
  "column-major" ordering, which is how R arrays are arranged in memory.
  'row' or 'C' will specify "row-major" order.

- compressor:

  What (if any) compression tool should be applied to the array chunks.
  The default is to use `zstd` compression. Supplying `NULL` will
  disable chunk compression. See
  [compressors](https://huber-group-embl.github.io/Rarr/reference/compressors.md)
  for more details.

- fill_value:

  The default value for uninitialized portions of the array. Does not
  have to be provided, in which case the default for the specified data
  type will be used.

- nchar:

  For character arrays this parameter gives the maximum length of the
  stored strings. If this argument is not specified the array provided
  to `x` will be checked and the length of the longest string found will
  be used so no data are truncated. However this may be slow and
  providing a value to `nchar` can provide a modest performance
  improvement.

- dimension_separator:

  The character used to to separate the dimensions in the names of the
  chunk files. Valid options are limited to "." and "/".

- zarr_version:

  The version of the Zarr specification to use. Currently, either `2` or
  `3`. The default is `3`.

## Value

The function is primarily called for the side effect of writing to disk.
Returns (invisibly) `TRUE` if the array is successfully written.

## Note

If `x` has `dimnames`, `names(dimnames(x))` will be stored as the
`dimension_names` field in the Zarr metadata.

## Examples

``` r

new_zarr_array <- file.path(tempdir(), "integer.zarr")
x <- array(1:50, dim = c(10, 5))
write_zarr_array(
  x = x, zarr_array_path = new_zarr_array,
  chunk_dim = c(2, 5)
)
```
