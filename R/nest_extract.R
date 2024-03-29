#' Extract a character column into multiple columns using regex groups in a column of nested data frames
#' 
#' @description
#' `nest_extract()` is used to extract capturing groups from a column in a nested 
#' data frame using regular expressions into a new column. If the groups don't 
#' match, or the input is NA, the output will be NA.
#' 
#' @return
#'  An object of the same type as `.data`. Each object in the column `.nest_data` 
#'  will have new columns created according to the capture groups specified in
#'  the regular expression.
#'  
#' @details
#'  `nest_extract()` is a wrapper for [tidyr::extract()] and maintains the functionality 
#'  of `extract()` within each nested data frame. For more information on `extract()` 
#'  please refer to the documentation in ['tidyr'](https://tidyr.tidyverse.org/).
#'  
#' @inheritParams nest_select
#' @inheritParams tidyr::extract
#' @param col Column name or position within `.nest_data` (must be present within
#'   all nested data frames in `.nest_data`). This is passed to `tidyselect::vars_pull()`.
#'   
#'   This argument is passed by expression and supports quasiquotation (you can
#'   unquote column names or column positions).
#' @param ... Additional arguments passed on to [tidyr::extract()] methods.
#' 
#' @importFrom dplyr enquos
#' @importFrom dplyr mutate
#' @importFrom rlang :=
#' @importFrom purrr map
#' @importFrom tidyr extract
#' 
#' @export
#' @family tidyr verbs
#' 
#' @examples
#' set.seed(123)
#' gm <- gapminder::gapminder 
#' 
#' gm <- 
#'   gm %>% 
#'   dplyr::mutate(comb = sample(c(NA, "a-b", "a-d", "b-c", "d-e"),
#'                               size = nrow(gm),
#'                               replace = TRUE))
#'                               
#' gm_nest <- gm %>% tidyr::nest(country_data = -continent)
#' 
#' gm_nest %>% 
#'   nest_extract(country_data,
#'                col = comb,
#'                into = c("var1","var2"),
#'                regex = "([[:alnum:]]+)-([[:alnum:]]+)")
nest_extract <- function(.data,
                          .nest_data,
                          col,
                          into,
                          regex = "([[:alnum:]]+)",
                          remove = TRUE,
                          convert = FALSE,
                          ...){
  
  # assertions and checks
  check_nest_data(.data, {{ .nest_data }})
  
  # tidyeval of dots
  dots <- dplyr::enquos(...)
  
  # map mutate over list-col
  dplyr::mutate(
    .data,
    "{{.nest_data}}" := purrr::map({{ .nest_data }}, 
                                   ~tidyr::extract(.x, col = {{ col }},into = into,regex = regex,
                                                    remove = remove,convert = convert, !!!dots))
  )
}