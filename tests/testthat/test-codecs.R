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
