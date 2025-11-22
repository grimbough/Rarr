# Changelog

## Rarr 1.11

### Minor improvements

- There is now a dedicated vignette describing the supported Zarr
  features in Rarr, available at
  <https://huber-group-embl.github.io/Rarr/articles/features.html>. This
  makes it more easily discoverable on the Bioconductor landing page.
- More data types are available when writing Zarr arrays:
  - boolean / logical
  - int8
  - int16
  - float32 / single

### Bug fixes

- Rarr is now fully compatible with big endian platforms.
- ZSTD decompression now also works in case where we cannot guess a
  priori the buffer size from the data type, such as when using variable
  length strings. Thanks to Artür Manukyan for the bug report and test
  data.
- [`zarr_overview()`](https://huber-group-embl.github.io/Rarr/reference/zarr_overview.md)
  no longer fails on consolidated metadata containing uncompressed
  arrays. This was introduced in
  <https://github.com/Huber-group-EMBL/Rarr/pull/45>. Thanks to Sharla
  Gelfand for reporting the issue and providing test data.

### Internal changes

- Some internal changes are preparing the transition to support Zarr v3:
  - “C” and “F” fill orders are now handled via a codec mechanism, which
    also supports a wider range of transpose operations.
  - The endian configuration is now handled via a codec.
- A GitHub Actions workflow has been added to occasionally test this
  package on a big endian platform.
- Bundled libraries have been updated:
  - blosc 1.20.1 -\> 1.21.6
  - snappy 1.1.1 -\> 1.2.2

## Rarr 1.9

### New features

- New functions to work with Zarr attributes have been added:
  - [`read_zarr_attributes()`](https://huber-group-embl.github.io/Rarr/reference/read_zarr_attributes.md)
    reads Zarr v2 and v3 attributes
  - [`write_zarr_attributes()`](https://huber-group-embl.github.io/Rarr/reference/write_zarr_attributes.md)
    only supports writing Zarr v2 attributes for now.
- This package now has a pkgdown website, available at
  <https://huber-group-embl.github.io/Rarr/>.
- Zarr v3 arrays are now supported for reading metadata via
  [`zarr_overview()`](https://huber-group-embl.github.io/Rarr/reference/zarr_overview.md).

### Breaking changes

- `zarr_overview(as_data_frame = TRUE)` now returns information more in
  line with the output of `zarr_overview(as_data_frame = FALSE)`. In
  particular:
  - a new `endianness` column has been added to indicate the byte order
    of the array data.
  - the `nchunks` column is now a list column specifying the number of
    chunks in each dimension, rather than a single integer giving the
    total number of chunks.

### Minor improvements

- An explicit error message is now given when attempting to read a Zarr
  array version 3. This version will be supported in a future release of
  Rarr.

### Bug fixes

- `.url_parse_other()` now accounts for port numbers in host name and
  colons in S3 buckets.
- [`writeZarrArray()`](https://huber-group-embl.github.io/Rarr/reference/ZarrRealizationSink.md)
  now allows writing character arrays, and no longer errors complaining
  about null ‘nchar’ argument value. Default of ‘nchar’ is now `NULL`.
- [`writeZarrArray()`](https://huber-group-embl.github.io/Rarr/reference/ZarrRealizationSink.md)
  no longer silently and incorrectly fills the last rows/columns when
  `dim` is not divisible by `chunk_dim`.
- The object name is no longer repeated (e.g., `name.zarrname.zarr`)
  when writing a Zarr array to a file in the current working directory.
- Invalid URLs for examples with S3 storage in
  [`read_zarr_array()`](https://huber-group-embl.github.io/Rarr/reference/read_zarr_array.md)
  and
  [`zarr_overview()`](https://huber-group-embl.github.io/Rarr/reference/zarr_overview.md)
  have been updated.
- [`read_zarr_array()`](https://huber-group-embl.github.io/Rarr/reference/read_zarr_array.md)
  no longer errors on arrays with numeric values other than float, int,
  uint and complex.
- [`zarr_overview()`](https://huber-group-embl.github.io/Rarr/reference/zarr_overview.md)
  now returns an explicit error message when the .zarray file is absent

### Internal changes

- Coding style throughout the package has been harmonized using the air
  tool. Contributors using RStudio, Positron or VS Code should have
  their code styled automatically on save.
- Continuous integration checks have been made stricter by setting
  `biocCheck()` error level to “error” rather than “never”, and R CMD
  check error level to “warning” rather than “error”.
- Static analysis via the lintr package is now performed on each push
  and PR. It should mostly be invisible to users but might result in
  slightly increased performance in some cases.
- The superseded httr dependency has been replaced with the lighter curl
  package, thus reducing the total number of dependencies for the
  package from 42 to 40.
- The unused stringr dependency has been removed, reducing the total
  number of dependencies for the package from 40 to 38.
- A minor PROTECT()/UNPROTECT() imbalance in the C code, exposed by
  rchk, has been fixed. It is not likely to cause problems in real-world
  situations but it could theoretically lead to crashes in some cases.
- Argument `path` in internal function `read_array_metadata()` has been
  renamed to `zarr_path` for consistency with other internal functions
- Some internal functions have been renamed with a leading dot, in line
  with the officially recommended style for Bioconductor packages.
- This package now uses testthat instead of tinytest as a testing
  framework. This comes with more utilities to handle snapshot tests and
  mocked tests.
- Function calls are now counted in tests to ensure we don’t repeatedly
  perform a task (in particular, an expensive I/O task) more often than
  necessary.

## Rarr 1.7

- Added [`path()`](https://rdrr.io/pkg/BiocGenerics/man/path.html)
  method for `ZarrArray` class that returns the location of the zarr
  array root.
- Removed used of non-API call `SETLENGTH` in C code.
- Small changes to compilation of internal blosc libraries to cope with
  the C23 compiler becoming the default in R-4.5.0

## Rarr 1.5

- Fixed bug when creating an empty array with a floating datatype. The
  fill value would be interpreted as an integer by `read_metadata()` and
  create and array of the wrong type.
- Fixed bug in
  [`update_zarr_array()`](https://huber-group-embl.github.io/Rarr/reference/update_zarr_array.md)
  when `NULL` was provided to one or more dimensions in the `index`
  argument. This was parsed incorrectly and the underlying zarr was not
  modified.
- Fixed bug in reading 64-bit integer arrays compressed with ZLIB or
  LZ4. The calculated decompression buffer size was too small and
  reading would fail. (Thanks to Dan Auerbach for the report:
  <https://github.com/grimbough/Rarr/issues/10>)
- Added support for the ZarrArray S4 class and the DelayedArray
  framework.
- Improvements to read and write performance.

## Rarr 1.3

- Added support for using the zstd compression library for reading and
  writing.

## Rarr 1.1

- Fixed bug when reading an array if the fill value in `.zarray` was
  `null`.
- Addressed bug in makevars where Rarr.so could be compiled before
  libblosc.a was ready. Also backported to Rarr 1.0.2. (Thanks to
  Michael Sumner for reporting this issue:
  <https://github.com/grimbough/Rarr/issues/5>)
- Corrected issue where fixed length string datatypes would be written
  with null terminators, resulting in strings that were one byte longer
  than the dtype value written in the `.zarray` metadata. Also
  backported to Rarr 1.0.3.
- Added support for reading and writing the fixed length Unicode
  datatype, and for reading variable length UTF-8 datatype.

## Rarr 0.99.9

- Response it initial package review (thanks
  [@Kayla-Morrell](https://github.com/Kayla-Morrell))
- Provided manual page examples for use\_\* compression filter
  functions.
- Add details of how example data in inst/extdata/zarr_examples was
  created.
- General code tidying

## Rarr 0.99.8

- Patch compression libraries to remove R CMD check warnings about C
  functions that might crash R or write to something other than the R
  console. Working in Linux only.

## Rarr 0.99.7

- Allow reading and writing chunks with GZIP compression.
- Add compression level arguments to several compression tools.

## Rarr 0.99.6

- Allow reading and writing chunks with no compression.
- Enable LZ4 compression for writing.
- Fix bug in blosc compression that could result in larger chunks than
  necessary.
- Improve speed of indexing when combining chunks into the final output
  array.

## Rarr 0.99.5

- Fixed bug when specifying nested chunks, where the chunk couldn’t be
  written unless the directory already existed.

## Rarr 0.99.4

- When writing chunks that overlap the array edge, even the undefined
  overhang region should be written to disk.

## Rarr 0.99.3

- Allow choice between column and row ordering when creating a Zarr
  array

## Rarr 0.99.2

- Catch bug when chunk files contain values outside the array extent.
- Add manual page issues identified by BBS

## Rarr 0.99.1

- Switch from aws.s3 to paws.storage for S3 data retrieval.

## Rarr 0.99.0

- Initial Bioconductor submission.

## Rarr 0.0.1

- Added a `NEWS.md` file to track changes to the package.
