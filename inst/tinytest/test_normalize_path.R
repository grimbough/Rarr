windows_paths <- c(
  "c:/foo/bar/baz.zarr",
  "d:\\foo\\bar\\baz.zarr",
  "https://s3.foo.com/bar/baz.zarr"
)
windows_normalized_paths <- c(
  "c:/foo/bar/baz.zarr/",
  "d:/foo/bar/baz.zarr/",
  "https://s3.foo.com/bar/baz.zarr/"
)

nix_paths <- c(
  "/foo/bar/baz.zarr",
  "foo/bar/baz.zarr",
  "../foo/bar/baz.zarr",
  "./foo/bar/baz.zarr",
  "https://s3.foo.com/bar/baz.zarr"
)
nix_normalized_paths <- c(
  "/foo/bar/baz.zarr/",
  "foo/bar/baz.zarr/",
  "../foo/bar/baz.zarr/",
  "./foo/bar/baz.zarr/",
  "https://s3.foo.com/bar/baz.zarr/"
)

for(i in seq_along(windows_paths)) {
  expect_identical(
    Rarr:::.normalize_array_path(windows_paths[i]),
    windows_normalized_paths[i]
  )
}

for(i in seq_along(nix_paths)) {
  expect_identical(
    Rarr:::.normalize_array_path(nix_paths[i]),
    nix_normalized_paths[i]
  )
}
