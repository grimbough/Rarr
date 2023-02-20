paths <- c(
  "c:/foo/bar/baz.zarr",
  "d:\\foo\\bar\\baz.zarr",
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
    "C:/foo/bar/baz.zarr/",
    paste0(getwd(), "/foo/bar/baz.zarr/"),
    paste0(normalizePath("../", winslash = "/"), "/foo/bar/baz.zarr/"),
    paste0(normalizePath("./", winslash = "/"), "/foo/bar/baz.zarr/"),
    "https://s3.foo.com/bar/baz.zarr/"
  )
  
  for(i in seq_along(paths)) {
    expect_identical( .normalize_array_path(paths[i]), expected_output[i] )
  }
  
}