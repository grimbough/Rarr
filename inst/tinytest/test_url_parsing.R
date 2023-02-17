
paths <- list(
  aws_host =  "https://DOC-EXAMPLE-BUCKET1.s3.us-west-2.amazonaws.com/puppy.png",
  aws_path =  "https://s3.us-west-2.amazonaws.com/DOC-EXAMPLE-BUCKET1/puppy.png",
  #embl_host = "https://rarr-testing.s3.embl.de/bz2.zarr/.zarray",
  embl_path = "https://s3.embl.de/rarr-testing/bz2.zarr/.zarray"
  
)

for(i in which(grepl("aws", names(paths))) ) {
  expect_silent(parsed <- Rarr:::.url_parse_aws(paths[[ i ]]))
  expect_equal(parsed$bucket,   "DOC-EXAMPLE-BUCKET1")
  expect_equal(parsed$hostname, "https://s3.amazonaws.com")
  expect_equal(parsed$object,   "puppy.png")
  expect_equal(parsed$region,   "us-west-2")
}

for(i in which(grepl("embl", names(paths))) ) {
  expect_silent(parsed <- Rarr:::.url_parse_other(paths[[ i ]]))
  expect_equal(parsed$bucket,   "rarr-testing")
  expect_equal(parsed$hostname, "https://s3.embl.de")
  expect_equal(parsed$object,   "bz2.zarr/.zarray")
  expect_equal(parsed$region,   "auto")
}
