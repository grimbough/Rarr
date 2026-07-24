# Initialize a Zarr group

Initialize a Zarr group

## Usage

``` r
write_zarr_group(
  zarr_path,
  group,
  zarr_version = if (has_metadata_v2) 2L else 3L
)
```

## Arguments

- zarr_path:

  A character vector of length 1. This provides the path to a Zarr
  store.

- group:

  A character vector of length 1. This provides the name of the group to
  create. If `""`, the root group will be created.

- zarr_version:

  The version of the Zarr specification to use. If a metadata file
  already exists, the version will be inferred from the file. Otherwise,
  the default is `3`.

## Details

Nested groups are created recursively. For example, if
`group = "foo/bar"`, then the group `foo` will be created first,
followed by the group `bar` inside of it.

## Examples

``` r
zarr_v2 <- withr::local_tempfile(fileext = ".zarr")
write_zarr_group(zarr_v2, "test/deep/nested/group", zarr_version = 2L)
```
