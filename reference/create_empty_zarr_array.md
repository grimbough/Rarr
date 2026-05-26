# Create an (empty) Zarr array

Create an (empty) Zarr array

## Usage

``` r
create_empty_zarr_array(
  zarr_array_path,
  dim,
  chunk_dim,
  data_type,
  order = c("F", "C"),
  compressor = use_zstd(),
  fill_value = NULL,
  nchar = NULL,
  dimension_separator = if (zarr_version == 2L) "." else "/",
  dimension_names = NULL,
  zarr_version = 3L
)
```

## Arguments

- zarr_array_path:

  Character vector of length 1 giving the path to the new Zarr array.

- dim:

  Dimensions of the new array. Should be a numeric vector with the same
  length as the number of dimensions.

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

  For `datatype = "character"` this parameter gives the maximum length
  of the stored strings. It is an error not to specify this for a
  character array, but it is ignored for other data types.

- dimension_separator:

  The character used to to separate the dimensions in the names of the
  chunk files. Valid options are limited to "." and "/".

- dimension_names:

  Optional character vector with the same length as `dim`.

- zarr_version:

  The version of the Zarr specification to use. Currently, either `2` or
  `3`. The default is `3`.

## Value

This function is primarily called for the side effect of initialising a
Zarr array location and creating the `.zarray` or `zarr.json` metadata
file. Returns (invisibly) the normalized path it wrote the metadata to.

## See also

[`write_zarr_array()`](https://huber-group-embl.github.io/Rarr/reference/write_zarr_array.md),
[`update_zarr_array()`](https://huber-group-embl.github.io/Rarr/reference/update_zarr_array.md)

## Examples

``` r

new_zarr_array <- file.path(tempdir(), "temp.zarr")
create_empty_zarr_array(new_zarr_array,
  dim = c(10, 20), chunk_dim = c(2, 5),
  data_type = "integer"
)
```
