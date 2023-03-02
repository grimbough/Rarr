# Rarr 0.99.5

Fixed bug when specifying nested chunks, where the chunk couldn't be written
unless the directory already existed.

# Rarr 0.99.4

When writing chunks that overlap the array edge, even the undefined overhang
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
