# Read the attributes associated with a Zarr array or group

Read the attributes associated with a Zarr array or group

## Usage

``` r
read_zarr_attributes(zarr_path, s3_client)
```

## Arguments

- zarr_path:

  A character vector of length 1. This provides the path to a Zarr array
  or group of arrays. This can either be on a local file system or on S3
  storage.

- s3_client:

  A list representing an S3 client. This should be produced by
  [`paws.storage::s3()`](https://paws-r.r-universe.dev/paws.storage/reference/s3.html).

## Value

A list containing the attributes. If the file containing attributes
(`.zattrs` for Zarr v2 or `zarr.json` for Zarr v3) exists but no
attributes are provided, an empty list is returned.
