# Decompress a chunk in memory

R has internal decompression tools for zlib, bz2 and lzma compression.
We use external libraries bundled with the package for blosc and lz4
decompression.

## Usage

``` r
.decompress_chunk(compressed_chunk, metadata)
```

## Arguments

- compressed_chunk:

  Raw vector holding the compressed bytes for this chunk.

- metadata:

  List produced by
  [`.read_array_metadata()`](https://huber-group-embl.github.io/Rarr/reference/dot-read_array_metadata.md)
  with the contents of the `.zarray` file.

## Value

An array with the number of dimensions specified in the Zarr metadata.
In most cases it will have the same size as the Zarr chunk, however in
the case of edge chunks, which overlap the extent of the array, the
returned chunk will be smaller.
