# Consolidate Zarr metadata files into a single file

This function reads all the metadata files in a Zarr store and
consolidates them into a single file. Thanks to this, a single request
can be made to retrieve all the elements and their related metadata for
a Zarr store, which is especially beneficial for remote stores like S3.

## Usage

``` r
zarr_consolidate_metadata(
  zarr_store_path,
  s3_client = NULL,
  action = c("write", "return"),
  overwrite = TRUE
)
```

## Arguments

- zarr_store_path:

  A character vector of length 1. This provides the path to a Zarr
  store.

- s3_client:

  A list representing an S3 client. This should be produced by
  [`paws.storage::s3()`](https://paws-r.r-universe.dev/paws.storage/reference/s3.html).

- action:

  A character string specifying the action to take with the consolidated
  metadata. If `"write"` (the default), the consolidated metadata will
  be written back to the Zarr store. If `"return"`, the consolidated
  metadata will be returned as a list without writing it back to the
  store. The latter is particularly useful for non-writable stores.

- overwrite:

  A logical value (default `TRUE`) indicating whether to overwrite
  existing consolidated metadata when `action` is `"write"`. If `FALSE`
  and consolidated metadata already exists, an error will be raised.

## Value

If `action` is `"return"`, a list containing the consolidated metadata.
Otherwise, the function is called for its side effect and `NULL` is
returned invisibly.

## Examples

``` r
# v2
zarr_v2 <- withr::local_tempfile(fileext = ".zarr")
dir.create(zarr_v2)
jsonlite::write_json(
  list("zarr_format" = 2L),
  file.path(zarr_v2, ".zgroup")
)
write_zarr_array(
  array(1:4, dim = c(2, 2)),
  file.path(zarr_v2, "array1"),
  chunk_dim = c(1, 2),
  zarr_version = 2L
)
write_zarr_array(
  array(c(3.14, 42.42, 12.96, 7.89), dim = c(2, 2)),
  file.path(zarr_v2, "array2"),
  chunk_dim = c(1, 2),
  zarr_version = 2L
)
write_zarr_attributes(
 file.path(zarr_v2, "array1"),
 list(description = "This is array 1")
)
zarr_consolidate_metadata(zarr_v2, action = "return")
#> $zarr_consolidated_format
#> [1] 1
#> 
#> $metadata
#> $metadata$.zgroup
#> $metadata$.zgroup$zarr_format
#> $metadata$.zgroup$zarr_format[[1]]
#> [1] 2
#> 
#> 
#> 
#> $metadata$`array1/.zarray`
#> $metadata$`array1/.zarray`$shape
#> $metadata$`array1/.zarray`$shape[[1]]
#> [1] 2
#> 
#> $metadata$`array1/.zarray`$shape[[2]]
#> [1] 2
#> 
#> 
#> $metadata$`array1/.zarray`$chunks
#> $metadata$`array1/.zarray`$chunks[[1]]
#> [1] 1
#> 
#> $metadata$`array1/.zarray`$chunks[[2]]
#> [1] 2
#> 
#> 
#> $metadata$`array1/.zarray`$dtype
#> [1] "|u1"
#> 
#> $metadata$`array1/.zarray`$fill_value
#> [1] 0
#> 
#> $metadata$`array1/.zarray`$dimension_separator
#> [1] "."
#> 
#> $metadata$`array1/.zarray`$order
#> [1] "F"
#> 
#> $metadata$`array1/.zarray`$zarr_format
#> [1] 2
#> 
#> $metadata$`array1/.zarray`$filters
#> NULL
#> 
#> $metadata$`array1/.zarray`$compressor
#> $metadata$`array1/.zarray`$compressor$id
#> [1] "zstd"
#> 
#> $metadata$`array1/.zarray`$compressor$level
#> [1] 0
#> 
#> 
#> 
#> $metadata$`array1/.zattrs`
#> $metadata$`array1/.zattrs`$description
#> [1] "This is array 1"
#> 
#> 
#> $metadata$`array2/.zarray`
#> $metadata$`array2/.zarray`$shape
#> $metadata$`array2/.zarray`$shape[[1]]
#> [1] 2
#> 
#> $metadata$`array2/.zarray`$shape[[2]]
#> [1] 2
#> 
#> 
#> $metadata$`array2/.zarray`$chunks
#> $metadata$`array2/.zarray`$chunks[[1]]
#> [1] 1
#> 
#> $metadata$`array2/.zarray`$chunks[[2]]
#> [1] 2
#> 
#> 
#> $metadata$`array2/.zarray`$dtype
#> [1] "<f8"
#> 
#> $metadata$`array2/.zarray`$fill_value
#> [1] 0
#> 
#> $metadata$`array2/.zarray`$dimension_separator
#> [1] "."
#> 
#> $metadata$`array2/.zarray`$order
#> [1] "F"
#> 
#> $metadata$`array2/.zarray`$zarr_format
#> [1] 2
#> 
#> $metadata$`array2/.zarray`$filters
#> NULL
#> 
#> $metadata$`array2/.zarray`$compressor
#> $metadata$`array2/.zarray`$compressor$id
#> [1] "zstd"
#> 
#> $metadata$`array2/.zarray`$compressor$level
#> [1] 0
#> 
#> 
#> 
#> 

zarr_consolidate_metadata(zarr_v2, action = "write")
zarr_overview(zarr_v2)
#> Type: Group of Arrays
#> Path: /tmp/RtmpTH8nr4/file1e242e61c3cd.zarr
#> Arrays:
#> ---
#>   Path: /tmp/RtmpTH8nr4/file1e242e61c3cd.zarr/array1
#>   Shape: 2 x 2
#>   Chunk Shape: 1 x 2
#>   No. of Chunks: 2 (2 x 1)
#>   Data Type: uint8
#>   Endianness: NA
#>   Compressor: zstd
#>   Attributes: yes
#> ---
#>   Path: /tmp/RtmpTH8nr4/file1e242e61c3cd.zarr/array2
#>   Shape: 2 x 2
#>   Chunk Shape: 1 x 2
#>   No. of Chunks: 2 (2 x 1)
#>   Data Type: float64
#>   Endianness: little
#>   Compressor: zstd
#>   Attributes: no
```
