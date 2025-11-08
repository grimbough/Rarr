# Print a summary of a Zarr array

When reading a Zarr array using
[`read_zarr_array()`](https://huber-group-embl.github.io/Rarr/reference/read_zarr_array.md)
it is necessary to know it's shape and size. `zarr_overview()` can be
used to get a quick overview of the array shape and contents, based on
the `.zarray` (Zarr v2) or `zarr.json` (Zarr v3) metadata file each
array contains.

## Usage

``` r
zarr_overview(zarr_array_path, s3_client, as_data_frame = FALSE)
```

## Arguments

- zarr_array_path:

  A character vector of length 1. This provides the path to a Zarr array
  or group of arrays. This can either be on a local file system or on S3
  storage.

- s3_client:

  A list representing an S3 client. This should be produced by
  [`paws.storage::s3()`](https://paws-r.r-universe.dev/paws.storage/reference/s3.html).

- as_data_frame:

  Logical determining whether the Zarr array details should be printed
  to screen (`FALSE`) or returned as a `data.frame` (`TRUE`) so they can
  be used computationally.

## Value

If `as_data_frame = FALSE` the function invisible returns `TRUE` if
successful. However it is primarily called for the side effect of
printing details of the Zarr array(s) to the screen. If
`as_data_frame = TRUE` then a `data.frame` containing details of the
array is returned.

## Details

The function currently prints the following information to the R
console:

- array path

- array shape and size

- chunk and size

- the number of chunks

- the datatype of the array

- codec used for data compression (if any)

If given the path to a group of arrays the function will attempt to
print the details of all sub-arrays in the group.

## Examples

``` r
## Using a local file provided with the package
z1 <- system.file("extdata", "zarr_examples", "row-first",
  "int32.zarr",
  package = "Rarr"
)

## read the entire array
zarr_overview(zarr_array_path = z1)
#> Type: Array
#> Path: /home/runner/work/_temp/Library/Rarr/extdata/zarr_examples/row-first/int32.zarr
#> Shape: 30 x 20 x 10
#> Chunk Shape: 10 x 10 x 5
#> No. of Chunks: 12 (3 x 2 x 2)
#> Data Type: int32
#> Endianness: little
#> Compressor: blosc

## using a file on S3 storage
# \donttest{
z2 <- "https://noaa-nwm-retro-v2-zarr-pds.s3.amazonaws.com/feature_id/"
zarr_overview(z2)
#> Type: Array
#> Path: https://noaa-nwm-retro-v2-zarr-pds.s3.amazonaws.com/feature_id
#> Shape: 2729077
#> Chunk Shape: 2729077
#> No. of Chunks: 1 (1)
#> Data Type: int32
#> Endianness: little
#> Compressor: blosc
# }
```
