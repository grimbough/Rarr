
expect_equal(.check_datatype(data_type = "integer"), "<i4")
expect_equal(.check_datatype(data_type = "double"), "<f8")
expect_equal(.check_datatype(data_type = "character", nchar = "10"), "|S10")

expect_equal(.check_datatype(fill_value = 4L), "<i4")
expect_equal(.check_datatype(fill_value = 4), "<f8")
expect_equal(.check_datatype(fill_value = "Test", nchar = 8), "|S8")

## data_type should have precedence when both are supplied and incompatible
expect_equal(.check_datatype(data_type = "integer", fill_value = 4), "<i4")
expect_equal(.check_datatype(data_type = "double", fill_value = "Test"), "<f8")
expect_equal(.check_datatype(data_type = "character", fill_value = 4L, nchar = 4), "|S4")

expect_error(.check_datatype(data_type = "not-a-data-type"))
expect_error(.check_datatype(data_type = "<i2"))
expect_error(.check_datatype(data_type = list(1:10)))
expect_error(.check_datatype(data_type = raw(10)))

expect_error(.check_datatype(data_type = "|S"), pattern = "The 'nchar' argument must be provided")
expect_error(.check_datatype(fill_value = "Testing"), pattern = "The 'nchar' argument must be provided")
expect_error(.check_datatype(fill_value = "Testing", nchar = -4), pattern = "The 'nchar' argument must be provided")

