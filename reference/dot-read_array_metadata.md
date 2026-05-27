# Read the `.zarray` or `zarr.json` metadata file associated with a Zarr array

Read the `.zarray` or `zarr.json` metadata file associated with a Zarr
array

## Usage

``` r
.read_array_metadata(zarr_path, s3_client = NULL, ...)
```

## Arguments

- zarr_path:

  A character vector of length 1. This provides the path to a Zarr array
  or group of arrays. This can either be on a local file system or on S3
  storage.

- s3_client:

  A list representing an S3 client. This should be produced by
  [`paws.storage::s3()`](https://paws-r.r-universe.dev/paws.storage/reference/s3.html).

- ...:

  Temporary fix for backwards compatibility. Will be removed soon.

## Value

A list containing the array metadata
