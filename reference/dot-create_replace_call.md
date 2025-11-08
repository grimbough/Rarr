# Create a string of the form `x[idx[[1]], idx[[2]]] <- y` for an array `x` where the number of dimensions is variable.

Create a string of the form `x[idx[[1]], idx[[2]]] <- y` for an array
`x` where the number of dimensions is variable.

## Usage

``` r
.create_replace_call(x_name, idx_name, idx_length, y_name)
```

## Arguments

- x_name:

  Name of the object to have items replaced

- idx_name:

  Name of the list containing the indices

- idx_length:

  Length of the list specified in `idx_name`

- y_name:

  Name of the object containing the replacement items

## Value

A character vector of length one containing the replacement commands.
This is expected to be passed to `parse() |> eval()`.
