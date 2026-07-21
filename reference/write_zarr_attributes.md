# Read the .zattrs file associated with a Zarr array or group

Read the .zattrs file associated with a Zarr array or group

## Usage

``` r
write_zarr_attributes(
  zarr_path,
  new.zattrs = list(),
  overwrite = TRUE,
  zarr_version = if (has_metadata_v2) 2L else 3L
)
```

## Arguments

- zarr_path:

  A character vector of length 1. This provides the path to a Zarr array
  or group.

- new.zattrs:

  a list inserted to .zattrs at the `path`.

- overwrite:

  if `TRUE` (the default), existing .zattrs elements will be overwritten
  by `new.zattrs`.

- zarr_version:

  The version of the Zarr specification to use. If a metadata file
  already exists, the version will be inferred from the file. Otherwise,
  the default is `3`.

## Value

Invisibly, the updated attributes as a named list. This is equivalent to
(but faster than) using
[`read_zarr_attributes()`](https://huber-group-embl.github.io/Rarr/reference/read_zarr_attributes.md)
after writing. If no attributes were present before, this is identical
to `new.zattrs`.

## Examples

``` r
z1 <- withr::local_tempdir(fileext = ".zarr")
write_zarr_attributes(z1, list(date = "2025-01-01", author = "Jane Doe"))
#> Warning: cannot open file '/tmp/RtmpAVkqTw/file1e7f7421421b.zarr//zarr.json': No such file or directory
#> Error in file(con, "w"): cannot open the connection
```
