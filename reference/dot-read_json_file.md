# Read a JSON file from local disk or S3

Read a JSON file from local disk or S3

## Usage

``` r
.read_json_file(path, s3_client = NULL)
```

## Arguments

- path:

  Full path (local or S3) to a JSON file.

- s3_client:

  An S3 client produced by
  [`paws.storage::s3()`](https://paws-r.r-universe.dev/paws.storage/reference/s3.html),
  or `NULL` for local files.

## Value

A list as returned by
[`jsonlite::read_json()`](https://jeroen.r-universe.dev/jsonlite/reference/read_json.html)
/
[`jsonlite::fromJSON()`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html).
