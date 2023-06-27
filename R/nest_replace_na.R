#' Replace NAs with specified values in a column of nested data frames
#' 
#' @description
#' `nest_replace_na()` is used to replace missing values in selected columns of 
#' nested data frames using values specified by column.
#' 
#' @return
#'  An object of the same type as `.data`. Each object in the column `.nest_data` 
#'  will have NAs replaced in the specified columns.
#'  
#' @details
#'  `nest_replace_na()` is a wrapper for [tidyr::replace_na()] and maintains the functionality 
#'  of `replace_na()` within each nested data frame. For more information on `replace_na()` 
#'  please refer to the documentation in ['tidyr'](https://tidyr.tidyverse.org/).
#'  
#' @inheritParams nest_select
#' @param replace A list of values, with one value for each column in that has `NA` values
#' to be replaced.
#' @param ... Additional arguments for [tidyr::replace_na()] methods. Currently unused.
#' 
#' @importFrom dplyr enquos
#' @importFrom dplyr mutate
#' @importFrom rlang :=
#' @importFrom purrr map
#' @importFrom tidyr replace_na
#' 
#' @export
#' @family tidyr verbs
#' 
#' @examples
#' set.seed(123)
#' gm <- 
#'   gapminder::gapminder %>% 
#'   dplyr::mutate(pop = dplyr::if_else(runif(dplyr::n()) >= 0.9,
#'                                      NA_integer_,
#'                                      pop))
#'                                      
#' gm_nest <- gm %>% tidyr::nest(country_data = -continent)
#' 
#' gm_nest %>% 
#'   nest_replace_na(.nest_data = country_data,
#'                   replace = list(pop = -500))
nest_replace_na <- function(.data,
                            .nest_data,
                            replace,
                            ...){
  
  # assertions and checks
  check_nest_data(.data, {{ .nest_data }})
  
  # tidyeval of dots
  dots <- dplyr::enquos(...)
  
  # map mutate over list-col
  dplyr::mutate(
    .data,
    "{{.nest_data}}" := purrr::map({{ .nest_data }}, ~tidyr::replace_na(.x, replace = replace, !!!dots))
  )
}