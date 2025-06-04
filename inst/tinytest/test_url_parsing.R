
paths <- list(
  aws_host =  "https://DOC-EXAMPLE-BUCKET1.s3.us-west-2.amazonaws.com/puppy.png",
  aws_path =  "https://s3.us-west-2.amazonaws.com/DOC-EXAMPLE-BUCKET1/puppy.png",
  #embl_host = "https://rarr-testing.s3.embl.de/bz2.zarr/.zarray",
  embl_path = "https://s3.embl.de/rarr-testing/bz2.zarr/.zarray",
  eopf_path_1 = "https://objectstore.eodc.eu:2222/e05ab01a9d56408d82ac32d69a5aae2a:202505-s02msil2a/17/products/cpm_v256/S2A_MSIL2A_20250517T085541_N0511_R064_T35QKA_20250517T112203.zarr/",
  eopf_path_2 = "https://objects.eodc.eu:443/e05ab01a9d56408d82ac32d69a5aae2a:202506-s01siwgrh/03/products/cpm_v256/S1C_IW_GRDH_1SDV_20250603T053151_20250603T053216_002614_0056AB_EE86.zarr"

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

for(i in which(grepl("eopf_path_1", names(paths))) ) {
  expect_silent(parsed <- Rarr:::.url_parse_other(paths[[ i ]]))
  expect_equal(parsed$bucket,   "e05ab01a9d56408d82ac32d69a5aae2a:202505-s02msil2a")
  expect_equal(parsed$hostname, "https://objectstore.eodc.eu:2222")
  expect_equal(parsed$object,   "17/products/cpm_v256/S2A_MSIL2A_20250517T085541_N0511_R064_T35QKA_20250517T112203.zarr/")
  expect_equal(parsed$region,   "auto")
}

for(i in which(grepl("eopf_path_2", names(paths))) ) {
  expect_silent(parsed <- Rarr:::.url_parse_other(paths[[ i ]]))
  expect_equal(parsed$bucket,   "e05ab01a9d56408d82ac32d69a5aae2a:202506-s01siwgrh")
  expect_equal(parsed$hostname, "https://objects.eodc.eu:443")
  expect_equal(parsed$object,   "03/products/cpm_v256/S1C_IW_GRDH_1SDV_20250603T053151_20250603T053216_002614_0056AB_EE86.zarr")
  expect_equal(parsed$region,   "auto")
}

expect_identical(Rarr:::.determine_s3_provider(paths[["aws_host"]]),  "aws")
expect_identical(Rarr:::.determine_s3_provider(paths[["embl_path"]]), "other")
expect_identical(Rarr:::.determine_s3_provider("https://foo.bar/baz"), "other")
expect_identical(Rarr:::.determine_s3_provider(paths[["eopf_path_1"]]), "other")
expect_identical(Rarr:::.determine_s3_provider(paths[["eopf_path_2"]]), "other")
