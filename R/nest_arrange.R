#' Arrange rows within a nested data frames by column values
#' 
#' @description 
#' `nest_arrange()` orders the rows of nested data frames by the values of 
#' selected columns. 
#' 
#' `nest_arrange()` is largely a wrapper for [dplyr::arrange()] and maintains 
#' the functionality of `arrange()` within each nested data frame. For more 
#' information on `arrange()`, please refer to the documentation in 
#' [`dplyr`](https://dplyr.tidyverse.org/). 
#' 
#' @inheritParams generic-params
#' @param ... Variables, or functions of variables. Use [dplyr::desc()] to sort 
#'   a variable in descending order.
#' @param .by_group If `TRUE`, will sort first by grouping variable. Applies to 
#'   grouped data frames only. 
#' 
#' @importFrom dplyr enquos
#' @importFrom dplyr mutate
#' @importFrom rlang :=
#' @importFrom purrr map
#' @importFrom dplyr arrange
#' 
#' @export
#' @family single table verbs
#' 
#' @examples 
#' \dontrun{
#' gm_nest <- gapminder::gapminder %>% tidyr::nest(country_data = -continent)
#' 
#' gm_nest %>% 
#'   nest_arrange(country_data, pop)
#' 
#' gm_nest %>%
#'   nest_arrange(country_data, desc(pop))
#' }
nest_arrange <- function(.data, 
                         .nest_data,
                         ...,
                         .by_group = FALSE) {
  
  # assertions and checks
  check_nest_data(.data, {{ .nest_data }})
  
  # tidyeval of dots
  dots <- dplyr::enquos(...)
  
  # map arrange over list-col
  dplyr::mutate(
    .data,
    "{{.nest_data}}" := purrr::map({{ .nest_data }}, ~dplyr::arrange(.x, !!!dots, .by_group = .by_group))
  )
  
}
