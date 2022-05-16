#' Count observations by group in nested data frames
#' 
#' @description 
#' `nest_count()` lets you quickly count the unique values of one or more variables
#' within a nested data frame or tibble.
#' 
#' `nest_add_count()` adds a column to nested data frames/tibbles with group-wise
#' counts.
#' 
#' These functions are largely wrappers around their `dplyr` counterparts - for
#' more information, please consult the documentation for `dplyr::count()`. 
#' 
#' @return 
#' A tibble or data frame. `nest_count()` and `nest_add_count()` group transiently,
#' so the nested data frames have the same groups as the input.
#' 
#' @param .data A data frame, data frame extension (e.g. a tibble), or a lazy 
#'   data frame (e.g. from dbplyr or dtplyr).
#' @param .nest_data A list-col containing data frames (or tibbles).
#' @param ... data masking variables to group by.
#' @param wt data masking frequency weights. Can be `NULL` or a variable:
#' 
#'   * If `NULL` (the default), counts the number of rows in each group.
#'   * If a variable, computes `sum(wt)` for each group.
#' @param sort If `TRUE`, will show the largest groups at the top.
#' @param name The name of the new column in the output.
#' 
#' @export
#' 
#' @importFrom dplyr enquos
#' @importFrom dplyr mutate
#' @importFrom rlang :=
#' @importFrom purrr map
#' @importFrom dplyr count
#' @importFrom rlang !!!
#' 
#' @examples 
#' \dontrun{
#' library(tidyverse)
#' library(gapminder)
#' 
#' # find the count of each country within each tibble
#' gapminder %>%
#'   nest(country_data = -continent) %>%
#'   nest_count(country_data, country)
#'   
#' # add a new column with the count of each country within each tibble
#' gapminder %>%
#'   nest(country_data = -continent) %>%
#'   nest_add_count(country_data, country) 
#' }
nest_count <- function(.data,
                       .nest_data,
                       ...,
                       wt = NULL,
                       sort = FALSE,
                       name = NULL) {
  
  # tidyeval of dots 
  dots <- dplyr::enquos(...)
  
  # map count over list-col
  dplyr::mutate(
    .data,
    "{{.nest_data}}" := purrr::map({{ .nest_data }}, ~dplyr::count(.x, !!!dots, wt = {{ wt }}, sort = sort, name = name))
  )
  
}

#' @export
#' @importFrom dplyr add_count
#' @rdname nest_count
nest_add_count <- function(.data,
                           .nest_data,
                           ...,
                           wt = NULL,
                           sort = FALSE,
                           name = NULL) {
  
  # tidyeval of dots
  dots <- dplyr::enquos(...)
  
  # map add_count over list-col
  dplyr::mutate(
    .data,
    "{{.nest_data}}" := purrr::map({{ .nest_data }}, ~dplyr::add_count(.x, !!!dots, wt = {{ wt }}, sort = sort, name = name))
  )
  
}
