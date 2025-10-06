expect_error(
  use_blosc(cname = "foo"),
  pattern = "cname argument must be one of"
)
