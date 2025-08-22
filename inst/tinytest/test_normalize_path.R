paths <- c(
  # Windows paths
  "c:/foo/bar/baz.zarr",
  "d:\\foo\\bar\\baz.zarr",
  "e:\\foo\\bar\\baz.zarr/",
  "z://foo//bar//baz.zarr",
  # Unix-like paths
  "/foo/bar/baz.zarr",
  "/foo///bar//baz.zarr",
  # URLs
  "https://s3.foo.com/bar/baz.zarr"
)
expected_normalized_paths <- c(
  "c:/foo/bar/baz.zarr/",
  "d:/foo/bar/baz.zarr/",
  "e:/foo/bar/baz.zarr/",
  "z:/foo/bar/baz.zarr/",
  "/foo/bar/baz.zarr/",
  "/foo/bar/baz.zarr/",
  "https://s3.foo.com/bar/baz.zarr/"
)
actual_normalized_paths <- vapply(
  paths,
  Rarr:::.normalize_array_path,
  character(1),
  USE.NAMES = FALSE
)

expect_identical(actual_normalized_paths, expected_normalized_paths)

# Paths that actually exist on our testing filesystem
existing_paths <- c(
  "~/foo/bar/baz.zarr",
  "../foo/bar/baz.zarr",
  "./foo/bar/baz.zarr",
  "foo/bar/baz.zarr",
  "foo/bar/baz.zarr/",
  file.path(tempdir(), "foo", "/bar//", "baz.zarr")
)
actual_normalized_existing_paths <- vapply(
  existing_paths,
  Rarr:::.normalize_array_path,
  character(1),
  USE.NAMES = FALSE
)

# https://github.com/markvanderloo/tinytest/issues/124
tinytest::expect_match(
  actual_normalized_existing_paths,
  "/([^(.|..)]+/)*foo/bar/baz.zarr/$"
)

expect_identical(
  Rarr:::.normalize_array_path("baz.zarr"),
  Rarr:::.normalize_array_path("./baz.zarr")
)