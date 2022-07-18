#' Subset distinct/unique rows within a nested data frame
#' 
#' @description 
#' `nest_distinct()` selects only unique/distinct rows in a nested data frame. 
#' 
#' @return 
#' An object of the same type as `.data`. Each object in the column `.nest_data` 
#' will also be of the same type as the input. Each object in `.nest_data` has
#' the following properties:
#' 
#' * Rows are a subset of the input but appear in the same order.
#' * Columns are not modified if `...` is empty or `.keep_all` is `TRUE`.
#'   Otherwise, `nest_distinct()` first calls `dplyr::mutate()` to create new 
#'   columns within each object in `.nest_data`.
#' * Groups are not modified.
#' * Data frame attributes are preserved.
#' 
#' @details 
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
#' gm_nest <- gapminder::gapminder %>% tidyr::nest(country_data = -continent)
#' 
#' gm_nest %>% nest_distinct(country_data, country)
#' gm_nest %>% nest_distinct(country_data, country, year)
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


