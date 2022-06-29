# ---------------------------general-test-functions-----------------------------
test_format <- function(f, ...) {
  
  # get function output and args
  x <- f(...)
  args <- dplyr::enquos(...)
  
  # check format of function return matches expected
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  expect_named(x, c("continent", "country_data"))
  expect_type(x$country_data, "list")
  expect_type(x$country_data[[1]], c("tbl_df", "tbl", "data.frame" || c("grouped_f", "tbl_df", "tbl", "data.frame")))
  
}
