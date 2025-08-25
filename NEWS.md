# Rarr 1.9.5

* add support for reading, writing and updating Zarr v2 attributes

# Rarr 1.9.4

* The object name is no longer repeated (e.g., `name.zarrname.zarr`) when
  writing a Zarr array to a file in the current working directory.

# Rarr 1.9.3

* Fixed a bug in `writeZarrArray()` when `dim` is not divisible by `chunk_dim`.

# Rarr 1.9.2

* `writeZarrArray()` now allows writing character arrays, and no longer error 
  complaining about null 'nchar' argument value. Default of 'nchar' is now
  `NULL`.

# Rarr 1.9

* Updated `.url_parse_other()` to account for port numbers in host name and 
  colons in bucket.

# Rarr 1.7

* Added `path()` method for `ZarrArray` class that returns the location of the
  zarr array root.
* Removed used of non-API call `SETLENGTH` in C code.
* Small changes to compilation of internal blosc libraries to cope with
  the C23 compiler becoming the default in R-4.5.0

# Rarr 1.5

* Fixed bug when creating an empty array with a floating datatype.  The fill
  value would be interpreted as an integer by `read_metadata()` and create 
  and array of the wrong type.
* Fixed bug in `update_zarr_array()` when `NULL` was provided to one or more
  dimensions in the `index` argument.  This was parsed incorrectly and the 
  underlying zarr was not modified.
* Fixed bug in reading 64-bit integer arrays compressed with ZLIB or LZ4.  
  The calculated decompression buffer size was too small and reading would 
  fail. (Thanks to Dan Auerbach for the report: 
  https://github.com/grimbough/Rarr/issues/10)
* Added support for the ZarrArray S4 class and the DelayedArray framework.
* Improvements to read and write performance.

# Rarr 1.3

* Added support for using the zstd compression library for reading and writing.

# Rarr 1.1

* Fixed bug when reading an array if the fill value in `.zarray` was `null`.  
* Addressed bug in makevars where Rarr.so could be compiled before libblosc.a
  was ready. Also backported to Rarr 1.0.2.
  (Thanks to Michael Sumner for reporting this issue:
  https://github.com/grimbough/Rarr/issues/5)
* Corrected issue where fixed length string datatypes would be written with
  null terminators, resulting in strings that were one byte longer than the
  dtype value written in the `.zarray` metadata. Also backported to Rarr 1.0.3.
* Added support for reading and writing the fixed length Unicode datatype, and 
  for reading variable length UTF-8 datatype.

# Rarr 0.99.9

* Response it initial package review (thanks @Kayla-Morrell)
* Provided manual page examples for use_* compression filter functions.
* Add details of how example data in inst/extdata/zarr_examples was created.
* General code tidying

# Rarr 0.99.8

* Patch compression libraries to remove R CMD check warnings about C functions 
that might crash R or write to something other than the R console. Working
in Linux only.

# Rarr 0.99.7

* Allow reading and writing chunks with GZIP compression.
* Add compression level arguments to several compression tools.

# Rarr 0.99.6

* Allow reading and writing chunks with no compression.
* Enable LZ4 compression for writing.
* Fix bug in blosc compression that could result in larger chunks than necessary.
* Improve speed of indexing when combining chunks into the final output array.

# Rarr 0.99.5

* Fixed bug when specifying nested chunks, where the chunk couldn't be written
unless the directory already existed.

# Rarr 0.99.4

* When writing chunks that overlap the array edge, even the undefined overhang
region should be written to disk.

# Rarr 0.99.3

* Allow choice between column and row ordering when creating a Zarr array

# Rarr 0.99.2

* Catch bug when chunk files contain values outside the array extent.
* Add manual page issues identified by BBS

# Rarr 0.99.1

* Switch from aws.s3 to paws.storage for S3 data retrieval.

# Rarr 0.99.0

* Initial Bioconductor submission.

# Rarr 0.0.1

* Added a `NEWS.md` file to track changes to the package.
