# Precompute index positions grouped by chunk

For each chunk touched by `index`, returns the positions (1-based)
within each dimension of `index` that fall inside that chunk, together
with the within-chunk indices needed to extract values from the chunk
array.

## Usage

``` r
.chunk_positions_by_chunk(index, metadata)
```

## Arguments

- index:

  A list of integer vectors, one per dimension, giving the requested
  array indices (1-based).

- metadata:

  List of array metadata as returned by
  [`.read_array_metadata()`](https://huber-group-embl.github.io/Rarr/reference/dot-read_array_metadata.md).
  Used to derive chunk name keys via `.create_chunk_names()`.

## Value

A named list keyed by chunk names (same format as
`.create_chunk_names()`, e.g. `"c/0/1/0"` for Zarr V3). Each element is
a list with two components:

- `positions`: a per-dimension list of integer vectors of positions into
  the corresponding `index` vector that map to that chunk.

- `index_in_chunk`: a per-dimension list of 1-based integer vectors
  giving the within-chunk coordinates corresponding to `positions`.
