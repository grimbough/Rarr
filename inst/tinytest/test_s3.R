path <- "https://www.test.com/bucket/file1"
s3_client <- Rarr:::.create_s3_client(path)

if(require(mockery)) {
  
  ## This ensures .get_credentials always returns an error, even if the 
  ## host machine has credentials available
  stub(where = Rarr:::.create_s3_client, 
       what = '.get_credentials',
       how = function(...) stop(), 
       depth = 2)

  ## we expect an "anonymous" credential if nothing is found  
  expect_true(s3_client$.internal$config$credentials$anonymous)
}