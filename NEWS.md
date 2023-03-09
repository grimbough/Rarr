# Rarr 0.99.7#8

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
