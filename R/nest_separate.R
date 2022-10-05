#' Separate a character column into multiple columns in a column of nested data frames
#' 
#' @description
#' `nest_separate()` is used to separate a single character column into multiple
#' columns using a regular expression or a vector of character positions in a 
#' list of nested data frames.
#' 
#' @return
#'  An object of the same type as `.data`. Each object in the column `.nest_data` 
#'  will have the specified column split according to the regular expression or
#'  the vector of character positions.
#'  
#' @details
#'  `nest_separate()` is a wrapper for [tidyr::separate()] and maintains the functionality 
#'  of `separate()` within each nested data frame. For more information on `separate()` 
#'  please refer to the documentation in ['tidyr'](https::/tidyr.tidyverse.org).
#'  
#' @inheritParams nest_select
#' @inheritParams tidyr::separate
#' @param col Column name or position within. Must be present in all data frames 
#'   in `.nest_data`. This is passed to [tidyselect::vars_pull()].
#'   
#'   This argument is passed by expression and supports quasiquotation (you can 
#'   unquote column names or column positions).
#'   
#' @param ... Additional arguments passed on to [tidyr::separate()] methods.
#' 
#' @importFrom dplyr enquos
#' @importFrom dplyr mutate
#' @importFrom rlang :=
#' @importFrom purrr map
#' @importFrom tidyr separate
#' 
#' @export
#' @family tidyr verbs
#' 
#' @examples
#' set.seed(123)
#' gm <- gapminder::gapminder %>% mutate(comb = paste(continent,year,sep = "-"))
#' gm_nest <- gm %>% tidyr::nest(country_data = -continent)
#' 
#' gm_nest %>% 
#'   nest_separate(.nest_data = country_data,
#'                 col = comb,
#'                 into = c("var1","var2"),
#'                 sep = "-")
nest_separate <- function(.data,
                          .nest_data,
                          col,
                          into,
                          sep = "[^[:alnum:]]+",
                          remove = TRUE,
                          convert = FALSE,
                          extra = "warn",
                          fill = "warn",
                          ...){
  
  # assertions and checks
  check_nest_data(.data, {{ .nest_data }})
  
  # tidyeval of dots
  dots <- dplyr::enquos(...)
  
  # map mutate over list-col
  dplyr::mutate(
    .data,
    "{{.nest_data}}" := purrr::map({{ .nest_data }}, 
                                   ~tidyr::separate(.x, col = {{ col }}, into = into, sep = sep,
                                                    remove = remove, convert = convert,
                                                    extra = extra, fill = fill, !!!dots))
  )
}