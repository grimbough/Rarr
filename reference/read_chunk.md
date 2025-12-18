# Read a single Zarr chunk

Read a single Zarr chunk

## Usage

``` r
read_chunk(
  zarr_array_path,
  chunk_name,
  metadata,
  s3_client = NULL,
  alt_chunk_dim = NULL,
  fill = FALSE
)
```

## Arguments

- zarr_array_path:

  A character vector of length 1, giving the path to the Zarr array

- chunk_name:

  The name of the chunk to read.

- metadata:

  List produced by
  [`.read_array_metadata()`](https://huber-group-embl.github.io/Rarr/reference/dot-read_array_metadata.md)
  holding the contents of the `.zarray` file. If missing this function
  will be called automatically, but it is probably preferable to pass
  the meta data rather than read it repeatedly for every chunk.

- s3_client:

  Object created by
  [`paws.storage::s3()`](https://paws-r.r-universe.dev/paws.storage/reference/s3.html).
  Only required for a file on S3. Leave as `NULL` for a file on local
  storage.

- alt_chunk_dim:

  The dimensions of the array that should be created from this chunk.
  Normally this will be the same as the chunk shape in `metadata`, but
  when dealing with edge chunks, which may overlap the true extent of
  the array the returned array should be smaller than the chunk shape.

- fill:

  Logical of length 1. If `TRUE`, missing chunks will be filled with the
  fill value from the array metadata. If `FALSE` (the default), missing
  chunks will return `NULL`.

## Value

An array containing the decompressed chunk values.
