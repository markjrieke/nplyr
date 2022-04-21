#' Arrange rows within a nested data frame by column values
#' 
#' @description 
#' `nest_arrange()` orders the rows of a nested data frame by the values of 
#' selected columns. This is largely a wrapper around `dplyr::arrange()` - please
#' refer to `dplyr` documentation for more details.
#' 
#' @return 
#' A tibble or dataframe. When printed to the console, the returned object will 
#' appear visually similar to the object passed to `nest_arrange()`; the rows
#' within each nested tibble/dataframe, however, will be reordered by the columns
#' passed to `...`. 
#' 
#' @param .data A data frame, data frame extension (e.g. a tibble), or a lazy 
#'   lazy data frame (e.g. from dbplyr or dtplyr).
#' @param .nest_data A list-col containing a data frames (or tibbles).
#' @param ... <[`data-masking`][dplyr_data_masking]> Variables, or functions of 
#'   variables. Use [desc()] to sort a variable in descending order.
#' @param .by_group If `TRUE`, will sort first by grouping variable. Applies to 
#'   grouped data frames only. 
#' 
#' @export
#' 
#' @importFrom dplyr enquos
#' @importFrom dplyr mutate
#' @importFrom purrr map
#' @importFrom dplyr arrange
#'   
#' @examples 
#' \dontrun{
#' library(gapminder)
#' library(tidyverse)
#' 
#' # create nested tibbles by continent from the gapminder dataset
#' # then arrange by population *within* each nested tibble.
#' gapminder %>%
#'   nest(country_data = -continent) %>%
#'   nest_arrange(country_data, pop)
#' }
nest_arrange <- function(.data, 
                         .nest_data,
                         ...,
                         .by_group = FALSE) {
  
  # tidyeval of dots
  dots <- dplyr::enquos(...)
  
  # map arrange over list-col
  dplyr::mutate(
    .data,
    "{{.nest_data}}" := purrr::map({{ .nest_data }}, ~dplyr::arrange(.x, !!!dots, .by_group = .by_group))
  )
  
}
