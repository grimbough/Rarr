test_that("scalar can be read", {
  # Not explicitly supported (but still used) in v2.
  # v3 spec says: "A shape can be the empty tuple in the case of zero-dimension arrays (scalars)."
  scalar_v2 <- system.file(
    "extdata",
    "zarr_examples",
    "scalar",
    "scalar_v2.zarr",
    package = "Rarr"
  )
  expect_identical(read_zarr_array(scalar_v2), structure("A string", dim = 1))

  scalar_v3 <- system.file(
    "extdata",
    "zarr_examples",
    "scalar",
    "scalar_v3.zarr",
    package = "Rarr"
  )
  expect_identical(read_zarr_array(scalar_v3), structure(42L, dim = 1))
})
