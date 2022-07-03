# ---------------------------general-test-functions-----------------------------
gm_nest <- readRDS("data/gm_nest.rds")

test_format <- function(f, ..., grouped = FALSE) {
  
  # get function output and args
  x <- f(...)
  args <- dplyr::enquos(...)
  
  # check format of function return matches expected
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  expect_named(x, c("continent", "country_data"))
  expect_type(x$country_data, "list")
  expect_tibble(x$country_data[[1]], grouped)
  
}

# check for either a tibble or grouped tibble class
expect_tibble <- function(object, grouped) {
  
  if (grouped == TRUE) {
    expect_s3_class(object, c("grouped_df", "tbl_df", "tbl", "data.frame"))
  } else {
    expect_s3_class(object, c("tbl_df", "tbl", "data.frame"))
  }
  
}
