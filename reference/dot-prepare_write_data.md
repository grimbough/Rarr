# Truncate overflow values and warn about bool NA values before writing

Applies the two pre-write data checks that are needed identically in
both
[`write_zarr_array()`](https://huber-group-embl.github.io/Rarr/reference/write_zarr_array.md)
and
[`update_zarr_array()`](https://huber-group-embl.github.io/Rarr/reference/update_zarr_array.md):

## Usage

``` r
.prepare_write_data(x, metadata)
```

## Arguments

- x:

  The R array to be written.

- metadata:

  Array metadata list as returned by
  [`.read_array_metadata()`](https://huber-group-embl.github.io/Rarr/reference/dot-read_array_metadata.md).

## Value

`x`, possibly with overflow values clamped.

## Details

1.  If the target type is narrower than R's native representation (e.g.
    `int8`, `int16`, `float32`), clamp out-of-range values via
    `.truncate_overflow()`.

2.  If the target type is `bool`, warn when `x` contains `NA` values
    (which the Zarr bool type cannot represent).
