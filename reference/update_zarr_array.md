# Update (a subset of) an existing Zarr array

Update (a subset of) an existing Zarr array

## Usage

``` r
update_zarr_array(zarr_array_path, x, index)
```

## Arguments

- zarr_array_path:

  Character vector of length 1 giving the path to the Zarr array that is
  to be modified.

- x:

  The R array (or object that can be coerced to an array) that will be
  written to the Zarr array.

- index:

  A list with the same length as the number of dimensions of the target
  array. This argument indicates which elements in the target array
  should be updated.

## Value

The function is primarily called for the side effect of writing to disk.
Returns (invisibly) `TRUE` if the array is successfully updated.

## Examples

``` r

## first create a new, empty, Zarr array
new_zarry_array <- file.path(tempdir(), "new_array.zarr")
create_empty_zarr_array(
  zarr_array_path = new_zarry_array, dim = c(20, 10),
  chunk_dim = c(10, 5), data_type = "double"
)

## create a matrix smaller than our Zarr array
small_matrix <- matrix(runif(6), nrow = 3)

## insert the matrix into the first 3 rows, 2 columns of the Zarr array
update_zarr_array(new_zarry_array, x = small_matrix, index = list(1:3, 1:2))

## reading back a slightly larger subset,
## we can see only the top left corner has been changed
read_zarr_array(new_zarry_array, index = list(1:5, 1:5))
#>           [,1]      [,2] [,3] [,4] [,5]
#> [1,] 0.2383798 0.7234506    0    0    0
#> [2,] 0.4116800 0.6145241    0    0    0
#> [3,] 0.7460717 0.4739806    0    0    0
#> [4,] 0.0000000 0.0000000    0    0    0
#> [5,] 0.0000000 0.0000000    0    0    0
```
