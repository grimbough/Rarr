# Convert special fill values from strings to numbers

Special case fill values (NaN, Inf, -Inf) are encoded as strings in the
Zarr metadata. R will create arrays of type character if these are
defined and the chunk isn't present on disk. This function updates the
fill value to be R's representation of these special values, so numeric
arrays are created. A "null" fill value implies no missing values. We
set this to NA as you can't create an array of type NULL in R. It should
have no impact if there are really no missing values.

## Usage

``` r
.update_fill_value(fill_value, datatype)
```

## Arguments

- fill_value:

  The fill value as read from the Zarr metadata.

- datatype:

  A list of details for the array datatype. Expected to be produced by
  [`grumpy::parse_npy_datatype()`](https://hugogruson.fr/grumpy/reference/parse_npy_datatype.html).

## Value

Returns a modified fill value. The returned value will be equal to the
input, but with the correct type, unless the `fill_value` entry was one
of: NULL, "NaN", "Infinity" or "-Infinity".
