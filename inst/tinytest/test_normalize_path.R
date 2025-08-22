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
normalized_paths <- c(
  "c:/foo/bar/baz.zarr/",
  "d:/foo/bar/baz.zarr/",
  "e:/foo/bar/baz.zarr/",
  "z:/foo/bar/baz.zarr/",
  "/foo/bar/baz.zarr/",
  "/foo/bar/baz.zarr/",
  "https://s3.foo.com/bar/baz.zarr/"
)

for(i in seq_along(paths)) {
  expect_identical(
    Rarr:::.normalize_array_path(paths[i]),
    normalized_paths[i]
  )
}

# Paths that actually exist on our testing filesystem
actual_paths <- c(
  "~/foo/bar/baz.zarr",
  "../foo/bar/baz.zarr",
  "./foo/bar/baz.zarr",
  "foo/bar/baz.zarr",
  "foo/bar/baz.zarr/",
  "baz.zarr",
  file.path(tempdir(), "foo", "/bar//", "baz.zarr")
)

# Specification states:
# After normalization, if splitting a logical path by the “/” character results
# in any path segment equal to the string “.” or the string “..” then an error
# MUST be raised.
for (i in seq_along(actual_paths)) {
  # https://github.com/markvanderloo/tinytest/issues/124
  tinytest::expect_match(
    Rarr:::.normalize_array_path(actual_paths[i]),
    "/([^(.|..)]+/)*foo/bar/baz.zarr/$"
  )
}
