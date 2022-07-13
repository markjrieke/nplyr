#' Subset rows in nested data frames using column values.
#' 
#' @description 
#' `nest_filter()` is used to subset nested data frames, retaining all rows that 
#' satisfy your conditions. To be retained, the row must produce a value of 
#' `TRUE` for all conditions. Note that when a condition evaluates to `NA` the 
#' row will be dropped, unlike base subsetting with `[`. 
#' 
#' `nest_filter()` subsets the rows within `.nest_data`, applying the 
#' expressions in `...` to the column values to determine which rows should be 
#' retained. It can be applied to both grouped and ungrouped data.
#' 
#' `nest_filter()` is largely a wrapper for [dplyr::filter()] and maintains the 
#' functionality of `filter()` within each nested data frame. For more 
#' information on `filter()`, please refer to the documentation in 
#' [`dplyr`](https://dplyr.tidyverse.org/).
#' 
#' @inheritParams nest_select
#' @param ... Expressions that return a logical value, and are defined in terms 
#'   of the variables in `.nest_data`. If multiple expressions are included, 
#'   they are combined with the `&` operator. Only rows for which all conditions
#'   evaluate to `TRUE` are kept.
#' @param .preserve Relevant when `.nest_data` is grouped. If 
#'   `.preserve = FALSE` (the default), the grouping structure is recalculated 
#'   based on the resulting data, otherwise the grouping is kept as is.
#' 
#' @importFrom dplyr enquos
#' @importFrom dplyr mutate
#' @importFrom rlang :=
#' @importFrom purrr map
#' @importFrom dplyr filter
#' 
#' @export
#' @family single table verbs
#' 
#' @examples 
#' \dontrun{
#' gm_nest <- gapminder::gapminder %>% tidyr::nest(country_data = -continent)
#' 
#' # apply a filter
#' gm_nest %>%
#'   nest_filter(country_data, year > 1972)
#' 
#' # apply multiple filters
#' gm_nest %>%
#'   nest_filter(country_data, year > 1972, pop < 10000000)
#'   
#' # apply a filter on grouped data
#' gm_nest %>%
#'   nest_group_by(country_data, country) %>%
#'   nest_filter(country_data, pop > mean(pop))
#' }
nest_filter <- function(.data,
                        .nest_data,
                        ...,
                        .preserve = FALSE) {
  
  # assertions and checks
  check_nest_data(.data, {{ .nest_data }})
  
  # tidyeval of dots 
  dots <- dplyr::enquos(...)
  
  # map filter over list-col
  dplyr::mutate(
    .data,
    "{{.nest_data}}" := purrr::map({{ .nest_data }}, ~dplyr::filter(.x, !!!dots, .preserve = .preserve))
  )
  
}