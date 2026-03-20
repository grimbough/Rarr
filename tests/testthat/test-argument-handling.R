test_that("use_blosc() accepts all configurable options", {
  compressor <- use_blosc(
    cname = "blosclz",
    clevel = 9L,
    shuffle = "bitshuffle",
    typesize = 8L,
    blocksize = 0L
  )

  expect_identical(compressor$id, "blosc")
  expect_identical(compressor$cname, "blosclz")
  expect_identical(compressor$clevel, 9L)
  expect_identical(compressor$shuffle, 2L)
  expect_identical(compressor$typesize, 8L)
  expect_identical(compressor$blocksize, 0L)
})
