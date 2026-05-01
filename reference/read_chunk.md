# Read a single Zarr chunk

Read a single Zarr chunk

## Usage

``` r
read_chunk(chunk_path, metadata, s3_client = NULL)
```

## Arguments

- chunk_path:

  A character vector of length 1, giving the path to the chunk to be
  read.

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

## Value

An array containing the decompressed chunk values.
