# -----------------------------common-assertions--------------------------------
check_nest_data <- function(.data, .nest_data) {
  
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
    assertthat::are_equal(class(x[1][[1]]), c("tbl_df", "tbl", "data.frame")),
    msg = "argument `.nest_data` msut be of class \"tbl_df\", \"tbl\", \"data.frame\"."
  )
  
}

