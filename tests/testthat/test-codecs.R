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

test_that("unsupported codec", {
  invalid_codec <- withr::local_tempfile(fileext = ".zarr")
  dir.create(invalid_codec)
  file.copy(
    system.file(
      "extdata",
      "zarr_examples",
      "column-first",
      "boolean_v3.zarr",
      package = "Rarr"
    ),
    invalid_codec,
    recursive = TRUE
  )
  meta <- jsonlite::read_json(
    file.path(invalid_codec, "boolean_v3.zarr", "zarr.json")
  )
  meta$codecs <- c(
    meta$codecs,
    list(
      list(
        "name" = "unsupported_codec",
        "configuration" = list()
      )
    )
  )
  jsonlite::write_json(
    meta,
    file.path(invalid_codec, "boolean_v3.zarr", "zarr.json"),
    auto_unbox = TRUE,
    pretty = TRUE
  )
  expect_error(
    Rarr::read_zarr_array(file.path(invalid_codec, "boolean_v3.zarr")),
    "not supported: unsupported_codec"
  )
})
