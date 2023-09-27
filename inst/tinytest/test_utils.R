
## different number of dimensions given to index than the array
expect_error(
  Rarr:::check_index(index = list(1), metadata = list(shape = c(10, 10)))
)

## negative indices
expect_error(
  Rarr:::check_index(index = list(-1, 1), metadata = list(shape = c(10, 10)))
)

## outside array extent
expect_error(
  Rarr:::check_index(index = list(100, 1), metadata = list(shape = c(10, 10)))
)
