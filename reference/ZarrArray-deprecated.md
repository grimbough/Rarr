# Deprecated DelayedArray backend functions

The DelayedArray backend has moved to a dedicated package: the ZarrArray
package (<https://github.com/Bioconductor/ZarrArray>).

## Usage

``` r
ZarrArray(...)

writeZarrArray(...)
```

## Arguments

- ...:

  Ignored.

## Examples

``` r
zarr_path <- system.file(
  "extdata",
  "zarr_examples",
  "column-first",
  "int32.zarr",
  package = "Rarr"
)
ZarrArray(
  zarr_path
)
#> Warning: The functions related to the DelayedArray backend have moved to the dedicated ZarrArray package (https://github.com/Bioconductor/ZarrArray) under the same name.
```
