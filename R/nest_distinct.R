#' Subset distinct/unique rows within a nested data frame
#' 
#' @description 
#' `nest_distinct()` selects only unique/distinct rows in a nested data frame. 
#' 
#' `nest_distinct()` is largely a wrapper for [dplyr::distinct()] and maintains 
#' the functionality of `distinct()` within each nested data frame. For more 
#' information on `distinct()`, please refer to the documentation in 
#' [`dplyr`](https://dplyr.tidyverse.org/). 
#' 
#' @inheritParams nest_select
#' @param ... Optional variables to use when determining uniqueness. If there 
#'   are multiple rows for a given combination of inputs, only the first row 
#'   will be preserved. If omitted, will use all variables.
#' @param .keep_all If `TRUE`, keep all variables in `.nest_data`. If a 
#' combination of `...` is not distinct, this keeps the first row of values. 
#' 
#' @importFrom dplyr enquos
#' @importFrom dplyr mutate
#' @importFrom rlang :=
#' @importFrom purrr map
#' @importFrom dplyr distinct
#' 
#' @export
#' 
#' @examples 
#' \dontrun{
#' gm_nest <- gapminder::gapminder %>% tidyr::nest(country_data = -continent)
#' 
#' gm_nest %>% nest_distinct(country_data, country)
#' gm_nest %>% nest_distinct(country_data, country, year)
#' }
nest_distinct <- function(.data,
                          .nest_data,
                          ...,
                          .keep_all = FALSE) {
  
  # assertions and checks
  check_nest_data(.data, {{ .nest_data }})
  
  # tidyeval of dots
  dots <- dplyr::enquos(...)
  
  # map distinct over list-col
  dplyr::mutate(
    .data,
    "{{.nest_data}}" := purrr::map({{ .nest_data }}, ~dplyr::distinct(.x, !!!dots, .keep_all = .keep_all))
  )
  
}


