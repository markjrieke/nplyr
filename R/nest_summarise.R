#' Summarise each group in nested data frames to fewer rows
#' 
#' @description 
#' `nest_summarise()` creates a new set of nested data frames. Each will have 
#' one (or more) rows for each combination of grouping variables; if there are 
#' no grouping variables, the output will have a single row summarising all 
#' observations in `.nest_data`. Each nested data frame will contain one column
#' for each grouping variable and one column for each of the summary statistics
#' that you have specified.
#' 
#' `nest_summarise()` and `nest_summarize()` are synonyms.
#' 
#' @details 
#' `nest_summarise()` is largely a wrapper for [dplyr::summarise()] and 
#' maintains the functionality of `summarise()` within each nested data frame. 
#' For more information on `summarise()`, please refer to the documentation in
#' [`dplyr`](https://dplyr.tidyverse.org/).
#' 
#' @inheritParams nest_select
#' @param ... Name-value pairs of functions. The name will be the name of the 
#'   variable in the result.
#'   
#'   The value can be:
#'   
#'   * A vector of length 1, e.g. `min(x)`, `n()`, or `sum(is.na(y))`.
#'   * A vector of length `n`, e.g., `quantile()`.
#'   * A data frame, to add multiple columns from a single expression.
#'   
#' @param .groups `r lifecycle::badge("experimental")` Grouping structure of the 
#'   result. Refer to [dplyr::summarise()] for more up-to-date information.
#' 
#' @importFrom dplyr enquos
#' @importFrom dplyr mutate
#' @importFrom rlang :=
#' @importFrom purrr map
#' @importFrom dplyr summarise
#' 
#' @export
#' 
#' @family single table verbs
#' 
#' @examples 
#' gm_nest <- gapminder::gapminder %>% tidyr::nest(country_data = -countinent)
#' 
#' # a summary applied to an ungrouped tbl returns a single row
#' gm_nest %>%
#'   nest_summarise(
#'     country_data,
#'     n = n(),
#'     median_pop = median(pop)
#'   )
#'
#' # usually, you'll want to group first
#' gm_nest %>%
#'   nest_group_by(country_data, country) %>%
#'   nest_summarise(
#'     country_data,
#'     n = n(),
#'     median_pop = median(pop)
#'   )
nest_summarise <- function(.data, 
                           .nest_data,
                           ...,
                           .groups = NULL) {
  
  # tidyeval of dots
  dots <- dplyr::enquos(...)
  
  # map summarise over list-col
  dplyr::mutate(
    .data,
    "{{.nest_data}}" := purrr::map({{ .nest_data }}, ~dplyr::summarise(.x, !!!dots, .groups = .groups))
  )
  
}

#' @export
#' @rdname nest_summarise
nest_summarize <- nest_summarise

