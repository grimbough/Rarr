# ZarrArray constructor

`ZarrayArray`: function to create a new object of class `ZarrArray`.

## Usage

``` r
ZarrArray(zarr_array_path)
```

## Arguments

- zarr_array_path:

  Path to a Zarr array. A character vector of length 1. This can either
  be a location on a local file system or the URI to an array in S3
  storage.

## Value

Object of class `ZarrArray`

## Examples

``` r
zarr_example <- system.file(
  "extdata", "zarr_examples", "column-first", "int32.zarr",
  package = "Rarr"
)
zarr_array <- ZarrArray(zarr_example)
is(zarr_array)
#> [1] "ZarrArray"         "DelayedArray"      "DelayedUnaryIsoOp"
#> [4] "DelayedUnaryOp"    "DelayedOp"         "Array"            
dim(zarr_array)
#> [1] 30 20 10
chunkdim(zarr_array)
#> [1] 10 10  5
```
