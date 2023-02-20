windows_paths <- c(
  "c:/foo/bar/baz.zarr",
  "d:\\foo\\bar\\baz.zarr",
  "/foo/bar/baz.zarr",
  "foo/bar/baz.zarr",
  "../foo/bar/baz.zarr",
  "./foo/bar/baz.zarr",
  "https://s3.foo.com/bar/baz.zarr"
)

nix_paths <- c(
  "/foo/bar/baz.zarr",
  "foo/bar/baz.zarr",
  "../foo/bar/baz.zarr",
  "./foo/bar/baz.zarr",
  "https://s3.foo.com/bar/baz.zarr"
)

if(.Platform$OS.type == "windows") {
  
  expected_output <- c(
    "c:/foo/bar/baz.zarr/",
    "d:/foo/bar/baz.zarr/",
    "https://s3.foo.com/bar/baz.zarr/"
  )
  
  for(i in seq_along(windows_paths)) {
    expect_identical( Rarr:::.normalize_array_path(windows_paths[i]), expected_output[i] )
  }
  
} else {
  
  expected_output <- c(
    "/foo/bar/baz.zarr/",
    "foo/bar/baz.zarr/",
    "../foo/bar/baz.zarr/",
    "./foo/bar/baz.zarr/",
    "https://s3.foo.com/bar/baz.zarr/"
  )
  
  for(i in seq_along(nix_paths)) {
    expect_identical( Rarr:::.normalize_array_path(nix_paths[i]), expected_output[i] )
  }
  
}


