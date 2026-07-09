# Changelog

## Rarr 2.1

### Breaking changes

- (Minor:) `s3_client =` argument default value in
  [`read_zarr_array()`](https://huber-group-embl.github.io/Rarr/reference/read_zarr_array.md),
  [`zarr_overview()`](https://huber-group-embl.github.io/Rarr/reference/zarr_overview.md),
  etc. is now set to `NULL` instead of missing. In practice, we expect
  this grant to be invisible to most users but it makes it easier to
  pass down missing values in Rarr reverse dependencies.
- The name and configuration options for the fixed-length-ascii (`|S` in
  Zarr v2) and fixed-length-ucs4 (`<U` or `>U` in Zarr v2) data types
  have been updated to `null_terminated_bytes` and `fixed_length_utf32`
  respectively to match their newly specified format in Zarr v3.
- Structured data types (record arrays) now always return lists as the
  internal elements, instead of vectors as previously. This allows
  structured data types to contain different data types in a single
  element.
- Unless `data_type` is specified explicitly, integers are now written
  using the smallest possible bitsize based on the array `x` range in
  [`write_zarr_array()`](https://huber-group-embl.github.io/Rarr/reference/write_zarr_array.md).

### New features

- [Zarr v3 struct
  datatype](https://github.com/zarr-developers/zarr-extensions/tree/main/data-types/struct)
  (equivalent to Zarr v2 structured datatype) is now supported.
  [Deprecated Zarr v3 structured
  datatype](https://github.com/zarr-developers/zarr-extensions/tree/main/data-types/structured)
  is implemented as well, but only for reading, as per specification for
  a deprecated type.
- The new
  [`zarr_consolidate_metadata()`](https://huber-group-embl.github.io/Rarr/reference/zarr_consolidate_metadata.md)
  function consolidates metadata of all elements under a given group in
  its associated `.zmetadata` (for Zarr v2 trees) or `zarr.json` (for
  Zarr v3 trees). Creating this consolidated metadata has two benefits:
  - better performance: the metadata of all elements under a group can
    be accessed more efficiently since a single file needs to be read
    instead of multiple smaller files.
  - easier direct access of all the elements in a remote S3 store, even
    though Rarr doesn’t have yet store-agnostic verbs to list, read,
    etc. elements.
- The `sharding_indexed` codec is now supported to read sharded Zarr
  arrays.
- [`zarr_overview()`](https://huber-group-embl.github.io/Rarr/reference/zarr_overview.md)
  now returns a new logical field `attributes` indicating whether each
  array has associated attributes.

### Minor improvements

- `normalize_array_path()` has been slightly optimized for speed. It is
  not likely to have a significant impact if you reading a single large
  array but can be noticed if you reading many attributes and small
  arrays (as in some anndata objects).
- Empty chunks, i.e., chunks were all elements are equal to the fill
  value, are no longer written by
  [`write_zarr_array()`](https://huber-group-embl.github.io/Rarr/reference/write_zarr_array.md),
  saving disk space, and improving performance when reading it back.
- More blosc options (`clevel`, `shuffle`, etc.) are exposed via
  [`use_blosc()`](https://huber-group-embl.github.io/Rarr/reference/compressors.md).
- Reading VLen-UTF8 arrays (used by default for `string` in v3) in now
  much faster after rewriting the vlen-utf8 codec in C.
- Unsupported data types are now caught explicitly and early early in
  the reading pipeline rather than potentially failing or returning
  incorrect output later.
- Chunks larger than the whole array in one or multiple dimensions are
  now permitted, based on a request by Artür Manukyan.
- 0 is now a valid, and the default, compression level for Zstd. In
  practice, it doesn’t have any effect because level 0 currently
  corresponds to level 3.
- Performance has been improved for writing and in the case where the
  `index` argument in
  [`read_zarr_array()`](https://huber-group-embl.github.io/Rarr/reference/read_zarr_array.md)
  is a continuous sequence. One such example is when the entire array is
  read (`index` argument missing).

### Bug fixes

- Using blosc compression via variable-length types such as when using
  the vlen-utf8 filter / codec, is no longer causing R to crash.
- [`zarr_overview()`](https://huber-group-embl.github.io/Rarr/reference/zarr_overview.md)
  and
  [`read_zarr_array()`](https://huber-group-embl.github.io/Rarr/reference/read_zarr_array.md)
  on Zarr v3 files hosted on S3. Thanks to a report and a patch by Artür
  Manukyan.
- [`create_empty_zarr_array()`](https://huber-group-embl.github.io/Rarr/reference/create_empty_zarr_array.md)
  and by extension
  [`write_zarr_array()`](https://huber-group-embl.github.io/Rarr/reference/write_zarr_array.md)
  now use the correct data type (`bool`) in metadata for boolean arrays.
  Thanks to a report by Artür Manukyan.

### Internal changes

- A refactor reinforced shared the use of internal functions handling
  indices across
  [`read_zarr_array()`](https://huber-group-embl.github.io/Rarr/reference/read_zarr_array.md),
  [`write_zarr_array()`](https://huber-group-embl.github.io/Rarr/reference/write_zarr_array.md),
  and
  [`update_zarr_array()`](https://huber-group-embl.github.io/Rarr/reference/update_zarr_array.md).
  Some redundant internal functions have been merged. This reduced the
  cyclomatic complexity in every function back to \<15 and it opens the
  door to further optimizations which now only need to take place in a
  single function.
- Some code duplication has been removed by moving metadata file
  existence in the lower-level shared utilities
  [`.read_array_metadata()`](https://huber-group-embl.github.io/Rarr/reference/dot-read_array_metadata.md)
  and
  [`.read_consolidated_metadata()`](https://huber-group-embl.github.io/Rarr/reference/dot-read_consolidated_metadata.md).
  While this is still discouraged, this also facilitates re-use of the
  internal functions in other packages (e.g., ZarrArray).
- Parsing Zarr v2 datatypes and the bytes codec decoding operation are
  now handled internally by the new [grumpy CRAN
  package](https://cran.r-project.org/package=grumpy).

## Rarr 1.99

### Breaking changes

- The DelayedArray backend
  ([`writeZarrArray()`](https://huber-group-embl.github.io/Rarr/reference/ZarrArray-deprecated.md)
  and
  [`ZarrArray()`](https://huber-group-embl.github.io/Rarr/reference/ZarrArray-deprecated.md)
  functions) has been migrated to a separate, dedicated package. This
  reduces the number of dependencies from 37 to 24. This also greatly
  improves performance in for the standard case (when the DelayedArray
  backend is not used).
- [`write_zarr_array()`](https://huber-group-embl.github.io/Rarr/reference/write_zarr_array.md)
  now writes Zarr v3 by default. Writing Zarr v2 is still possible by
  explicitly setting the argument `zarr_version = 2`.

### New features

- Zarr v3 arrays with data types and codecs that already existed in v2
  can now be read via
  [`read_zarr_array()`](https://huber-group-embl.github.io/Rarr/reference/read_zarr_array.md),
  and written via
  [`write_zarr_array()`](https://huber-group-embl.github.io/Rarr/reference/write_zarr_array.md).
- Zarr v3 consolidated metadata is now returned by
  [`zarr_overview()`](https://huber-group-embl.github.io/Rarr/reference/zarr_overview.md),
  the same way it was already previously done for v2 consolidated
  metadata.
- More data types are available when writing Zarr arrays:
  - boolean / logical
  - int8
  - int16
  - int64 (up to values that can be represented as R integers)
  - uint8
  - uint16
  - uint32 (up to values that can be represented as R integers)
  - uint64 (up to values that can be represented as R integers)
  - float32 / single
- Scalar arrays (i.e., arrays with zero dimensions) can now be read.
  Thanks to Artür Manukyan for the bug report.
- Zarr attributes can now be read by passing an s3 URL directly as the
  first argument of
  [`read_zarr_attributes()`](https://huber-group-embl.github.io/Rarr/reference/read_zarr_attributes.md).
  This makes
  [`read_zarr_attributes()`](https://huber-group-embl.github.io/Rarr/reference/read_zarr_attributes.md)
  consistent with
  [`read_zarr_array()`](https://huber-group-embl.github.io/Rarr/reference/read_zarr_array.md)
  and
  [`zarr_overview()`](https://huber-group-embl.github.io/Rarr/reference/zarr_overview.md).
- “Simple” structured data types (i.e., only one level of nesting and no
  arrays) can now be read from Zarr v2 arrays.
- `simplifyVector = FALSE` is added to `fromJSON` in
  [`read_zarr_attributes()`](https://huber-group-embl.github.io/Rarr/reference/read_zarr_attributes.md),
  thus attributes of both local and s3 zarr stores are read identically.
- The `dimension_names` optional field is support in both v2 (not
  strictly part of the spec) and v3. It is mapped to
  `names(dimnames(.))` in R.
- `NA_real_` is now an allowed fill value in
  [`write_zarr_array()`](https://huber-group-embl.github.io/Rarr/reference/write_zarr_array.md)
  when writing numeric arrays, following a request from Hervé Pagès.
- Fill values stored as their byte representation are now understood
  when reading Zarr arrays.
- [`write_zarr_array()`](https://huber-group-embl.github.io/Rarr/reference/write_zarr_array.md)
  now supports writing `NA_character_`, which means it is possible to
  preserve `NA`s when roundtriping an R character array, based on a
  request from Hervé Pagès.

### Minor improvements

- There is now a dedicated vignette describing the supported Zarr
  features in Rarr, available at
  <https://huber-group-embl.github.io/Rarr/articles/features.html>. This
  makes it more easily discoverable on the Bioconductor landing page.
- Rarr initializes empty/missing chunks only once per read operation,
  which significantly improves performance when reading arrays with many
  missing chunks.
- Reading fixed-length string and unicode arrays is now ~20% faster.
- The `shape` and `chunks` fields in v2 metadata are now always encoded
  as JSON arrays, even when they contain a single element. This makes
  Rarr more compatible with other Zarr implementations. Thanks to Artür
  Manukyan for the bug report and pull request.
- Empty zarr arrays (i.e., arrays with `shape` and `chunks` equal zero)
  can now be written.
- Compression for writing Zarr arrays now default to zstd rather than
  zlib. zstd achieves similar or better compression levels while being
  much faster at compressing (= writing Zarr arrays) and decompressing
  (= reading Zarr arrays). This matches the default used by Zarr Python
  implementation.
- [`write_zarr_array()`](https://huber-group-embl.github.io/Rarr/reference/write_zarr_array.md)
  now fails early with an explicit error message when `x` is not an
  array.

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
- the `fill_value` is now correctly interpreted when reading Zarr v2
  string or unicode arrays. This is visible for example when trying to
  read missing chunks from such arrays. Thanks to Artür Manukyan for the
  bug report.

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
  - zstd 1.5.5 -\> 1.5.7
  - lz4 1.9.2 -\> 1.10.0
- Resizable vector in C code for compression now uses the official
  exported R C API, instead of internal R functions.
- The `const` qualifier is used where appropriate in the C code.

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
- [`writeZarrArray()`](https://huber-group-embl.github.io/Rarr/reference/ZarrArray-deprecated.md)
  now allows writing character arrays, and no longer errors complaining
  about null ‘nchar’ argument value. Default of ‘nchar’ is now `NULL`.
- [`writeZarrArray()`](https://huber-group-embl.github.io/Rarr/reference/ZarrArray-deprecated.md)
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

- Added `path()` method for `ZarrArray` class that returns the location
  of the zarr array root.
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
