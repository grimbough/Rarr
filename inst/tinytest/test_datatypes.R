
expect_equal(.check_datatype(data_type = "integer"), "<i4")
expect_equal(.check_datatype(data_type = "double"), "<f8")
expect_equal(.check_datatype(data_type = "character"), "|S")

expect_equal(.check_datatype(fill_value = 4L), "<i4")
expect_equal(.check_datatype(fill_value = 4), "<f8")
expect_equal(.check_datatype(fill_value = "Test"), "|S")

## data_type should win when both are supplied and incompatible
expect_equal(.check_datatype(data_type = "integer", fill_value = 4), "<i4")
expect_equal(.check_datatype(data_type = "double", fill_value = "Test"), "<f8")
expect_equal(.check_datatype(data_type = "character", fill_value = 4L), "|S")
