#' Subset rows in a nested data frame using column values.
#' 
#' @description 
#' `nest_filter()` is used to subset a nested data frame, retaining all the rows
#' that satisfy your conditions. To be retained, the row must produce a value of 
#' `TRUE` for all conditions. Note that when a condition evaluates to `NA` the 
#' row will be dropped, unlike base subsetting with `[`. 
#' 
#' This function is largely a wrapper around its `dplyr` counterpart - for more
#' information, please consult the documentation for `dplyr::filter()`.
#' 
#' @param .data A data frame, data frame extension (e.g. a tibble) or a lazy
#'   data frame (e.g. from dbplyr or dtplyr). 
#' @param .nest_data A list-col containing data frames (or tibbles). 
#' @param ... `<data-masking>` Expressions that return a logical value and are 
#'   defined in terms of the variables in `.data`. If multiple expressions are
#'   included, they are combined with the `&` operator. Only rows for which all 
#'   conditions are `TRUE` are kept. Columns to filter by must be present in all
#'   nested tibbles in `.nest_data`. 
#' @param .preserve Relevant when the `.nest_data` input is grouped. If 
#'   `.preserve = FALSE` (the default), the grouping structure is recalculated 
#'   based on the resulting data, otherwise the grouping is kept as is.
#' 
#' @details 
#' The `nest_filter()` function is used to subset the rows of each of the objects
#' of `.nest_data`, applying the expressions in `...` to the column values to 
#' determine which rows should be retained. It can be applied to both grouped and
#' ungrouped data (see [nest_group_by()] and [nest_ungroup()]). However, the 
#' underlying filter operation from dplyr is not yet smart enough to optimise the
#' filtering operation on grouped datasets that do not need grouped calculations.
#' For this reason, filtering is often considerably faster on ungrouped data.
#' 
#' @return 
#' A tibble or dataframe with nested objects of the same type as `.nest_data`. 
#' The output in `.nest_data` has the following properties:
#' 
#' * Rows are a subset of the input, but appear in the same order.
#' * Columns are not modified.
#' * The number of groups may be reduced (if `.preserved` is not `TRUE`).
#' * Data frame attributes are preserved. 
#' 
#' @section Useful filter functions:
#' 
#' There are many functions and operators that are useful when constructing the
#' expressions used to filter the data:
#' 
#' * [`==`], [`>`], [`>=`], etc.
#' * [`&`], [`|`], [`!`], [`xor()`]
#' * [is.na()]
#' * [`dplyr::between()`], [`dplyr::near()`]
#' 
#' @section Grouped tibbles:
#' 
#' Because filtering expressions are computed within groups, they may yield 
#' different results on grouped tibbles. This will be the case as soon as an 
#' aggregating, lagging, or ranking function is involved. Compare this ungrouped
#' filtering:
#' 
#' ```
#' gapminder %>% 
#'   nest(data = -continent) %>% 
#'   nest_filter(data, pop > mean(pop, na.rm = TRUE))
#' ```
#' 
#' With the grouped equivalent:
#' 
#' ```
#' gapminder %>% 
#'   nest(data = -continent) %>%
#'   nest_group_by(data, country) %>%
#'   nest_filter(data, pop > mean(pop, na.rm = TRUE))
#' ```
#' 
#' In the ungrouped version, `nest_filter()` compares the value of `pop` in each
#' row to the global average in `.nest_data`, keeping only the rows with `pop` 
#' greater than this global average. In contrast, the grouped version calculates 
#' the average population separately for each `country` group within `.nest_data`, and 
#' keeps rows with `pop` greater than the relevant within-country average.
#' 
#' @export
#' 
#' @importFrom dplyr enquos
#' @importFrom dplyr mutate
#' @importFrom rlang :=
#' @importFrom purrr map
#' @importFrom dplyr filter
#' @importFrom rlang !!!
#' 
#' @examples 
#' \dontrun{
#' library(tidyverse)
#' library(gapminder)
#' 
#' # create nested tibble for use in examples
#' gapminder_nested <-
#'   gapminder %>%
#'   nest(country_data = -continent)
#' 
#' # filter by one criterion 
#' gapminder_nested %>% nest_filter(country_data, year == 1992)
#' gapminder_nested %>% nest_filter(country_data, pop < 10000000)
#' 
#' # filtering by multiple criteria within a single logical expression
#' gapminder_nested %>% nest_filter(country_data, year > 1990 & lifeExp < 40)
#' gapminder_nested %>% nest_filter(country_data, lifeExp > 40 | gdpPercap > 900)
#' 
#' # when multiple expressions are used, they are combined using &
#' gapminder_nested %>% nest_filter(country_data, year == 1992, pop < 10000000)
#' 
#' # filtering may yield different results on grouped tibbles 
#' # because the expressions are computed within groups within `.nest_data`.
#' 
#' # the following filters rows where `pop` is greater than the 
#' # global average within `.nest_data`.
#' gapminder_nested %>% 
#'   nest_filter(country_data, pop > mean(pop, na.rm = TRUE))
#' 
#' # whereas this keeps rows within greater than the 
#' # country average within `.nest_data`.
#' gapminder_nested %>% 
#'   nest_group_by(country_data, country) %>% 
#'   nest_filter(country_ata, pop > mean(pop, na.rm = TRUE))
#' }
nest_filter <- function(.data,
                        .nest_data,
                        ...,
                        .preserve = FALSE) {
  
  # tidyeval of dots 
  dots <- dplyr::enquos(...)
  
  # map filter over list-col
  dplyr::mutate(
    .data,
    "{{.nest_data}}" := purrr::map({{ .nest_data }}, ~dplyr::filter(.x, !!!dots, .preserve = .preserve))
  )
  
}