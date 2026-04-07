# Deprecated DelayedArray backend functions

**\[superseded\]**

The DelayedArray backend has moved to a dedicated package: the ZarrArray
package (<https://github.com/Bioconductor/ZarrArray>).

## Usage

``` r
ZarrArray(...)

writeZarrArray(...)
```

## Arguments

- ...:

  Passed to the new function in the ZarrArray package.

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
#> Warning: `ZarrArray()` was deprecated in Rarr 1.12.0.
#> ℹ Please use `ZarrArray::ZarrArray()` instead.
#> ℹ The functions related to the DelayedArray backend have moved to the dedicated
#>   ZarrArray package (https://github.com/Bioconductor/ZarrArray).
```
