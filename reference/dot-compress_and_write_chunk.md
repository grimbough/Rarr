# Compress and write a single chunk

Compress and write a single chunk

## Usage

``` r
.compress_and_write_chunk(
  input_chunk,
  chunk_path,
  metadata,
  compressor = use_zlib(),
  data_type_size,
  is_base64 = FALSE
)
```

## Arguments

- input_chunk:

  Array containing the chunk data to be compressed. Will be converted to
  a raw vector before compression.

- chunk_path:

  Character string giving the path to the chunk that should be written.

- compressor:

  A "compressor" function that returns a list giving the details of the
  compression tool to apply. See
  [compressors](https://huber-group-embl.github.io/Rarr/reference/compressors.md)
  for more details.

- data_type_size:

  An integer giving the size of the original datatype. This is passed to
  the blosc algorithm, which seems to need it to achieve any
  compression.

- is_base64:

  When dealing with Py_unicode strings we convert them to base64 strings
  for storage in our intermediate R arrays. This argument indicates if
  base64 is in use, because the conversion to raw in .as_raw should be
  done differently for base64 strings vs other types.

## Value

Returns `TRUE` if writing is successful. Mostly called for the
side-effect of writing the compressed chunk to disk.
