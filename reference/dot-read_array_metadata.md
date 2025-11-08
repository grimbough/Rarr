# Read the .zarray metadata file associated with a Zarr array

Read the .zarray metadata file associated with a Zarr array

## Usage

``` r
.read_array_metadata(zarr_path, metadata_file, s3_client = NULL)
```

## Arguments

- zarr_path:

  A character vector of length 1. This provides the path to a Zarr array
  or group of arrays. This can either be on a local file system or on S3
  storage.

- metadata_file:

  One of `".zarray"` (Zarr v2) or `"zarr.json"` (Zarr v3) specifying
  which metadata file to read.

- s3_client:

  A list representing an S3 client. This should be produced by
  [`paws.storage::s3()`](https://paws-r.r-universe.dev/paws.storage/reference/s3.html).

## Value

A list containing the array metadata
