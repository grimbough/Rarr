test_that("check_index errors for mismatched dimensions", {
  expect_error(
    Rarr:::check_index(index = list(1), metadata = list(shape = c(10, 10)))
  )
})


test_that("check_index errors for negative indices", {
  expect_error(
    Rarr:::check_index(index = list(-1, 1), metadata = list(shape = c(10, 10)))
  )
})


test_that("check_index errors for indices outside extent", {
  expect_error(
    Rarr:::check_index(index = list(100, 1), metadata = list(shape = c(10, 10)))
  )
})
