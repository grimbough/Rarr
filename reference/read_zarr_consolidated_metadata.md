# Read consolidated metadata

Read consolidated metadata

## Usage

``` r
read_zarr_consolidated_metadata(
  zarr_path,
  consolidate = c("never", "missing", "always"),
  s3_client = NULL
)
```

## Arguments

- zarr_path:

  A character vector of length 1. This provides the path to a Zarr array
  or group of arrays. This can either be on a local file system or on S3
  storage.

- consolidate:

  One of `"never"` (default), `"missing"` or `"always"`. This controls
  whether the consolidated metadata needs to be (re)generated on the
  fly, in memory. If `"never"` then the consolidated metadata is only
  read if it already exists, and `NULL` is returned otherwise. If
  `"missing"` then the consolidated metadata is generated if it does not
  already exist. If `"always"` then the consolidated metadata is always
  (re)generated, even if it already exists.

- s3_client:

  A list representing an S3 client. This should be produced by
  [`paws.storage::s3()`](https://paws-r.r-universe.dev/paws.storage/reference/s3.html).

## Value

A list containing the consolidated metadata, formatted as Zarr v3
consolidated metadata. If `consolidate = "never"` and the consolidated
metadata does not exist, `NULL` is returned.

## Details

This is stored in the `.zmetadata` (Zarr v2) or `zarr.json` (Zarr v3)
file at the root of a Zarr store.

Note that it is not documented in the official Zarr specification,
because it is not (yet?) part of the standard.

In particular, it lists the location of all the metadata files for
arrays in the current group, so it is not necessary to crawl to discover
them. It leads to much better performance, in particular when reading
arrays from S3 storage.

## References

<https://zarr.readthedocs.io/en/latest/user-guide/consolidated_metadata.html>

## Examples

``` r
z <- system.file("extdata", "zarr_examples", "metadata", "consolidated.zarr", package = "Rarr")
m <- read_zarr_consolidated_metadata(z)
head(m)
#> $a
#> $a$node_type
#> [1] "array"
#> 
#> $a$zarr_format
#> [1] 2
#> 
#> $a$datatype
#> $a$datatype$endian
#> [1] "little"
#> 
#> $a$datatype$base_type
#> [1] "float"
#> 
#> $a$datatype$nbytes
#> [1] 8
#> 
#> 
#> $a$data_type
#> [1] "float64"
#> 
#> $a$shape
#> $a$shape[[1]]
#> [1] 1
#> 
#> 
#> $a$chunk_grid
#> $a$chunk_grid$name
#> [1] "regular"
#> 
#> $a$chunk_grid$configuration
#> $a$chunk_grid$configuration$chunk_shape
#> $a$chunk_grid$configuration$chunk_shape[[1]]
#> [1] 1
#> 
#> 
#> 
#> 
#> $a$chunk_key_encoding
#> $a$chunk_key_encoding$name
#> [1] "default"
#> 
#> $a$chunk_key_encoding$configuration
#> $a$chunk_key_encoding$configuration$separator
#> [1] "."
#> 
#> 
#> 
#> $a$fill_value
#> [1] 0
#> 
#> $a$codecs
#> $a$codecs$bytes
#> $a$codecs$bytes$name
#> [1] "bytes"
#> 
#> $a$codecs$bytes$configuration
#> $a$codecs$bytes$configuration$endian
#> [1] "little"
#> 
#> 
#> 
#> $a$codecs$blosc
#> $a$codecs$blosc$name
#> [1] "blosc"
#> 
#> $a$codecs$blosc$configuration
#> $a$codecs$blosc$configuration$cname
#> [1] "lz4"
#> 
#> $a$codecs$blosc$configuration$clevel
#> [1] 5
#> 
#> $a$codecs$blosc$configuration$shuffle
#> [1] 1
#> 
#> $a$codecs$blosc$configuration$blocksize
#> [1] 0
#> 
#> 
#> 
#> 
#> $a$attributes
#> named list()
#> 
#> 
#> $b
#> $b$node_type
#> [1] "array"
#> 
#> $b$zarr_format
#> [1] 2
#> 
#> $b$datatype
#> $b$datatype$endian
#> [1] "little"
#> 
#> $b$datatype$base_type
#> [1] "float"
#> 
#> $b$datatype$nbytes
#> [1] 8
#> 
#> 
#> $b$data_type
#> [1] "float64"
#> 
#> $b$shape
#> $b$shape[[1]]
#> [1] 2
#> 
#> $b$shape[[2]]
#> [1] 2
#> 
#> 
#> $b$chunk_grid
#> $b$chunk_grid$name
#> [1] "regular"
#> 
#> $b$chunk_grid$configuration
#> $b$chunk_grid$configuration$chunk_shape
#> $b$chunk_grid$configuration$chunk_shape[[1]]
#> [1] 2
#> 
#> $b$chunk_grid$configuration$chunk_shape[[2]]
#> [1] 2
#> 
#> 
#> 
#> 
#> $b$chunk_key_encoding
#> $b$chunk_key_encoding$name
#> [1] "default"
#> 
#> $b$chunk_key_encoding$configuration
#> $b$chunk_key_encoding$configuration$separator
#> [1] "."
#> 
#> 
#> 
#> $b$fill_value
#> [1] 0
#> 
#> $b$codecs
#> $b$codecs$transpose
#> $b$codecs$transpose$name
#> [1] "transpose"
#> 
#> $b$codecs$transpose$configuration
#> $b$codecs$transpose$configuration$order
#> [1] 0 1
#> 
#> 
#> 
#> $b$codecs$bytes
#> $b$codecs$bytes$name
#> [1] "bytes"
#> 
#> $b$codecs$bytes$configuration
#> $b$codecs$bytes$configuration$endian
#> [1] "little"
#> 
#> 
#> 
#> $b$codecs$blosc
#> $b$codecs$blosc$name
#> [1] "blosc"
#> 
#> $b$codecs$blosc$configuration
#> $b$codecs$blosc$configuration$cname
#> [1] "lz4"
#> 
#> $b$codecs$blosc$configuration$clevel
#> [1] 5
#> 
#> $b$codecs$blosc$configuration$shuffle
#> [1] 1
#> 
#> $b$codecs$blosc$configuration$blocksize
#> [1] 0
#> 
#> 
#> 
#> 
#> $b$attributes
#> named list()
#> 
#> 
#> $c
#> $c$node_type
#> [1] "array"
#> 
#> $c$zarr_format
#> [1] 2
#> 
#> $c$datatype
#> $c$datatype$endian
#> [1] "little"
#> 
#> $c$datatype$base_type
#> [1] "float"
#> 
#> $c$datatype$nbytes
#> [1] 8
#> 
#> 
#> $c$data_type
#> [1] "float64"
#> 
#> $c$shape
#> $c$shape[[1]]
#> [1] 3
#> 
#> $c$shape[[2]]
#> [1] 3
#> 
#> $c$shape[[3]]
#> [1] 3
#> 
#> 
#> $c$chunk_grid
#> $c$chunk_grid$name
#> [1] "regular"
#> 
#> $c$chunk_grid$configuration
#> $c$chunk_grid$configuration$chunk_shape
#> $c$chunk_grid$configuration$chunk_shape[[1]]
#> [1] 3
#> 
#> $c$chunk_grid$configuration$chunk_shape[[2]]
#> [1] 3
#> 
#> $c$chunk_grid$configuration$chunk_shape[[3]]
#> [1] 3
#> 
#> 
#> 
#> 
#> $c$chunk_key_encoding
#> $c$chunk_key_encoding$name
#> [1] "default"
#> 
#> $c$chunk_key_encoding$configuration
#> $c$chunk_key_encoding$configuration$separator
#> [1] "."
#> 
#> 
#> 
#> $c$fill_value
#> [1] 0
#> 
#> $c$codecs
#> $c$codecs$transpose
#> $c$codecs$transpose$name
#> [1] "transpose"
#> 
#> $c$codecs$transpose$configuration
#> $c$codecs$transpose$configuration$order
#> [1] 0 1 2
#> 
#> 
#> 
#> $c$codecs$bytes
#> $c$codecs$bytes$name
#> [1] "bytes"
#> 
#> $c$codecs$bytes$configuration
#> $c$codecs$bytes$configuration$endian
#> [1] "little"
#> 
#> 
#> 
#> $c$codecs$blosc
#> $c$codecs$blosc$name
#> [1] "blosc"
#> 
#> $c$codecs$blosc$configuration
#> $c$codecs$blosc$configuration$cname
#> [1] "lz4"
#> 
#> $c$codecs$blosc$configuration$clevel
#> [1] 5
#> 
#> $c$codecs$blosc$configuration$shuffle
#> [1] 1
#> 
#> $c$codecs$blosc$configuration$blocksize
#> [1] 0
#> 
#> 
#> 
#> 
#> $c$attributes
#> named list()
#> 
#> 
```
