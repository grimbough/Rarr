# Read consolidated metadata file

Read consolidated metadata file

## Usage

``` r
.read_zmetadata(zarr_path, s3_client)
```

## Arguments

- zarr_path:

  A character vector of length 1. This provides the path to a Zarr array
  or group of arrays. This can either be on a local file system or on S3
  storage.

- s3_client:

  A list representing an S3 client. This should be produced by
  [`paws.storage::s3()`](https://paws-r.r-universe.dev/paws.storage/reference/s3.html).

## Details

This is stored in the `.zmetadata` file at the root of a Zarr store.
Note that it is not documented in the official Zarr specification,
because it is not (yet?) part of the standard.

It is implemented in zarr-python and discussed under the "consolidated
metadata" phrase.

In particular, it lists the location of all the metadata files for
arrays in the current group, so it is not necessary to crawl to discover
them.

## References

<https://zarr.readthedocs.io/en/latest/user-guide/consolidated_metadata.html>
