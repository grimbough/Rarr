# Apply Zarr v3-specific fixups to raw array metadata

Called after reading a `zarr.json` file for an array node. Performs four
normalisation steps that are needed before the metadata can be used by
the rest of the package:

## Usage

``` r
.normalize_v3_metadata(metadata)
```

## Arguments

- metadata:

  A list as returned by
  [`.read_json_file()`](https://huber-group-embl.github.io/Rarr/reference/dot-read_json_file.md)
  for a Zarr v3 array node (i.e. `metadata$zarr_format == 3L` and
  `metadata$node_type == "array"`).

## Value

The modified `metadata` list.

## Details

1.  Parse the `data_type` string into an R-friendly `datatype` list.

2.  Name `codecs` by their `name` field for O(1) lookup.

3.  Normalise scalar arrays (empty `shape`) to shape `1`.

4.  Inject a default `transpose` codec when absent (R uses F-order so we
    always need to know the intended order).

5.  Replicate the `endian` field across all fields of struct datatypes
    so that downstream chunk-reading code can treat it uniformly.
