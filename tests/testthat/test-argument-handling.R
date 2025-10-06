test_that("use_blosc() validates cname argument", {
  expect_error(
    use_blosc(cname = "foo"),
    pattern = "cname argument must be one of"
  )
})
