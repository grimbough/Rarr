# This is a modified version of paws.storage:::get_credentials(). It is included to prevent using the `:::` operator. Look at that function if things stop working.

This is a modified version of paws.storage:::get_credentials(). It is
included to prevent using the `:::` operator. Look at that function if
things stop working.

## Usage

``` r
.get_credentials(credentials)
```

## Arguments

- credentials:

  Content stored at `.internal$config$credentials` in an object created
  by
  [`paws.storage::s3()`](https://paws-r.r-universe.dev/paws.storage/reference/s3.html).

## Value

A credentials list to be reinserted into a `paws.storage` s3 object. If
no valid credentials are found this function will error, which is
expected and is caught by `.check_credentials`.
