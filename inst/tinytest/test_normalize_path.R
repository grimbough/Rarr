paths <- c(
  # Windows paths
  "c:/foo/bar/baz.zarr",
  "d:\\foo\\bar\\baz.zarr",
  "e:\\foo\\bar\\baz.zarr/",
  "z://foo//bar//baz.zarr",
  # Unix-like paths
  "/foo/bar/baz.zarr",
  "foo/bar/baz.zarr",
  "foo/bar/baz.zarr/",
  "../foo/bar/baz.zarr",
  "./foo/bar/baz.zarr",
  ".//foo///bar/baz.zarr",
  # Actually existing paths
  "baz.zarr",
  file.path(tempdir(), "foo", "/bar//", "baz.zarr"),
  # URLs
  "https://s3.foo.com/bar/baz.zarr"
)
normalized_paths <- c(
  "c:/foo/bar/baz.zarr/",
  "d:/foo/bar/baz.zarr/",
  "e:/foo/bar/baz.zarr/",
  "z:/foo/bar/baz.zarr/",
  "/foo/bar/baz.zarr/",
  "foo/bar/baz.zarr/",
  "foo/bar/baz.zarr/",
  "../foo/bar/baz.zarr/",
  "./foo/bar/baz.zarr/",
  "./foo/bar/baz.zarr/",
  "baz.zarr/",
  file.path(tempdir(), "foo", "bar", "baz.zarr", ""),
  "https://s3.foo.com/bar/baz.zarr/"
)

for(i in seq_along(paths)) {
  expect_identical(
    Rarr:::.normalize_array_path(paths[i]),
    normalized_paths[i]
  )
}
