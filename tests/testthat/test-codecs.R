test_that("codec: transpose", {
  x <- array(1:60, dim = c(3, 4, 5))

  expect_identical(
    x |> codec_transpose_encode(c(1, 2, 3)),
    x
  )

  ind <- c(2, 3, 1)
  expect_identical(
    x |> codec_transpose_encode(ind) |> codec_transpose_decode(ind),
    x
  )

  ind <- c(3, 1, 2)
  expect_identical(
    x |> codec_transpose_encode(ind) |> codec_transpose_decode(ind),
    x
  )

  ind <- c(1, 3, 2)
  expect_identical(
    x |> codec_transpose_encode(ind) |> codec_transpose_decode(ind),
    x
  )
})

test_that("codec: endian", {
  x <- runif(100)

  raw_le <- writeBin(x, con = raw(), size = 8, endian = "little")
  raw_be <- writeBin(x, con = raw(), size = 8, endian = "big")

  if (.Platform$endian == "little") {
    expect_identical(
      raw_le |> codec_bytes_encode("big", 8),
      raw_be
    )
  } else {
    expect_identical(
      raw_be |> codec_bytes_encode("little", 8),
      raw_le
    )
  }
})
