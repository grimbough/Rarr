# Compress and write a single chunk

Compress and write a single chunk

## Usage

``` r
.compress_and_write_chunk(input_chunk, chunk_path, metadata)
```

## Arguments

- input_chunk:

  Array containing the chunk data to be compressed. Will be converted to
  a raw vector before compression.

- chunk_path:

  Character string giving the path to the chunk that should be written.

## Value

Returns `TRUE` if writing is successful. Mostly called for the
side-effect of writing the compressed chunk to disk.
