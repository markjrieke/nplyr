# common assertions ------------------------------------------------------------
check_nest_data <- function(.data, .nest_data) {
  
  # check that .data is not rowwise
  assertthat::assert_that(
    !"rowwise_df" %in% class(.data),
    msg = paste0("argument `.data` must not be a rowwise dataframe.\n",
                 "try calling `dplyr::ungroup()`")
  )
  
  x <- dplyr::pull(.data, {{ .nest_data }})
  
  if (!missing(x)) {
    
    assert_nest_df(x)
    
  } else {
    
    rlang::abort("argument \".nest_data\" is missing, with no default")
    
  }
  
}

assert_nest_df <- function(x) {
  
  assertthat::assert_that(
    assertthat::are_equal(class(x), "list"),
    msg = "argument `.nest_data` must be of class \"list\"."
  )
  
  assertthat::assert_that(
    assertthat::are_equal(class(x[1][[1]]), c("tbl_df", "tbl", "data.frame")) ||
      assertthat::are_equal(class(x[1][[1]]), c("grouped_df", "tbl_df", "tbl", "data.frame")) ||
      assertthat::are_equal(class(x[1][[1]]), c("spec_tbl_df", "tbl_df", "tbl", "data.frame")),
    msg = "argument `.nest_data` msut be of class \"grouped_df\", \"tbl_df\", \"tbl\", \"data.frame\"."
  )
  
}

