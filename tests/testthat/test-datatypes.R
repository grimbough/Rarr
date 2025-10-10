test_that("check_datatype works with data_type argument", {
  expect_identical(
    .check_datatype(data_type = "integer")[["data_type"]],
    "<i4"
  )
  expect_identical(
    .check_datatype(data_type = "double")[["data_type"]],
    "<f8"
  )
  expect_identical(
    .check_datatype(data_type = "character", nchar = "10")[[
      "data_type"
    ]],
    "|S10"
  )
})

test_that("check_datatype works with fill_value argument", {
  expect_identical(.check_datatype(fill_value = 4L)[["data_type"]], "<i4")
  expect_identical(.check_datatype(fill_value = 4)[["data_type"]], "<f8")
  expect_identical(
    .check_datatype(fill_value = "Test", nchar = 8)[["data_type"]],
    "|S8"
  )
})

test_that("data_type has precedence over fill_value when incompatible", {
  expect_identical(
    .check_datatype(data_type = "integer", fill_value = 4)[[
      "data_type"
    ]],
    "<i4"
  )
  expect_identical(
    .check_datatype(data_type = "double", fill_value = "Test")[[
      "data_type"
    ]],
    "<f8"
  )
  expect_identical(
    .check_datatype(
      data_type = "character",
      fill_value = 4L,
      nchar = 4
    )[[
      "data_type"
    ]],
    "|S4"
  )
})

test_that("check_datatype throws errors for invalid inputs", {
  expect_error(.check_datatype(data_type = "not-a-data-type"))
  expect_error(.check_datatype(data_type = "<f2"))
  expect_error(.check_datatype(data_type = list(1:10)))
  expect_error(.check_datatype(data_type = raw(10)))
})

test_that("check_datatype requires nchar for character types", {
  expect_error(
    .check_datatype(data_type = "|S"),
    "The 'nchar' argument must be provided"
  )
  expect_error(
    .check_datatype(fill_value = "Testing"),
    "The 'nchar' argument must be provided"
  )
  expect_error(
    .check_datatype(fill_value = "Testing", nchar = -4),
    "The 'nchar' argument must be provided"
  )
})
