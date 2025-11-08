# Determine the size of chunk in bytes after decompression

Determine the size of chunk in bytes after decompression

## Usage

``` r
get_decompressed_chunk_size(datatype, dimensions)
```

## Arguments

- datatype:

  A list of details for the array datatype. Expected to be produced by
  [`.parse_datatype()`](https://huber-group-embl.github.io/Rarr/reference/dot-parse_datatype.md).

- dimensions:

  A list containing the dimensions of the chunk. Expected to be found in
  a list produced by
  [`.read_array_metadata()`](https://huber-group-embl.github.io/Rarr/reference/dot-read_array_metadata.md).

## Value

An integer giving the size of the chunk in bytes
