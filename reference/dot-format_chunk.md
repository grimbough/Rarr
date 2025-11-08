# Format the decompressed chunk as an array of the correct type

When a chunk is decompressed it is returned as a vector of raw bytes.
This function uses the array metadata to select how to convert the bytes
into the final datatype and then converts the resulting output into an
array of the appropriate dimensions, including re-ordering if the
original data is in row-major order.

## Usage

``` r
.format_chunk(decompressed_chunk, metadata, alt_chunk_dim)
```

## Arguments

- decompressed_chunk:

  Raw vector holding the decompressed bytes for this chunk.

- metadata:

  List produced by
  [`.read_array_metadata()`](https://huber-group-embl.github.io/Rarr/reference/dot-read_array_metadata.md)
  holding the contents of the `.zarray` file.

- alt_chunk_dim:

  The dimensions of the array that should be created from this chunk.
  Normally this will be the same as the chunk shape in `metadata`, but
  when dealing with edge chunks, which may overlap the true extent of
  the array, the returned array should be smaller than the chunk shape.

## Value

A list of length 2. The first element is the formatted chunk data. The
second is an integer of length 1, indicating if warnings were
encountered when converting types

If "chunk_data" is larger than the space remaining in destination array
i.e. it contains the overflowing elements, these will be trimmed when
the chunk is returned to `read_data()`
