# Convert the metadata `codecs` elements into functions

Convert the metadata `codecs` elements into functions

## Usage

``` r
.configure_codecs(codecs, operation = c("encode", "decode"))
```

## Arguments

- codecs:

  A list containing the Zarr v3 codecs

- operation:

  One of "encode" or "decode"

## Value

An list of 3 environments containing functions:

- `bytes_bytes_codecs`: functions to encode/decode raw bytes

- `array_array_codecs`: functions to encode/decode R arrays

- `array_bytes_codecs`: functions to encode/decode between R arrays and
  raw bytes
