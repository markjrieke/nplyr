#' Subset distinct/unique rows within a nested tibble or dataframe
#' 
#' @description
#' Select only unique/distinct rows from a nested data frame.
#' 
#' @return 
#' A tibble or dataframe. 
#' 
#' @param .data A data frame, data frame extension (e.g. a tibble), or a lazy 
#'   lazy data frame (e.g. from dbplyr or dtplyr).
#' @param .nest_data A list-col containing a data frames (or tibbles).
#' @param ... Data masking optional variables to use when determining uniqueness.
#'   if there are multiple rows for a given combination of inputs, only the first
#'   row will be preserved. If omitted, will use all variables.
#' @param .keep_all If `TRUE`, keep all variables in each nested data frame passed 
#'   to `.nest_data`. If a combination of `...` is not distinct, this keeps the 
#'   first row of values.
#'   
#' @export
#' 
#' @importFrom dplyr enquos
#' @importFrom dplyr mutate
#' @importFrom rlang :=
#' @importFrom purrr map
#' @importFrom dplyr distinct
#' @importFrom rlang !!!
#' 
#' @examples 
#' \dontrun{
#' library(tidyverse)
#' library(gapminder)
#' 
#' # return a distinct list of countries within each continent
#' gapminder %>%
#'   nest(country_data = -continent) %>%
#'   nest_distinct(country_data, country)
#' }
nest_distinct <- function(.data,
                          .nest_data,
                          ...,
                          .keep_all = FALSE) {
  
  # tidyeval of dots
  dots <- dplyr::enquos(...)
  
  # map distinct over list-col
  dplyr::mutate(
    .data,
    "{{.nest_data}}" := purrr::map({{ .nest_data }}, ~dplyr::distinct(.x, !!!dots, .keep_all = .keep_all))
  )
  
}


