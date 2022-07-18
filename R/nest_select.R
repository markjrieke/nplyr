#' Subset columns in nested data frames using their names and types
#' 
#' @description 
#' `nest_select()` selects (and optionally renames) variables in nested data 
#' frames, using a concise mini-language that makes it easy to refer to 
#' variables based on their name (e.g., `a:f` selects all columns from `a` on 
#' the left to `f` on the right). You can also use predicate functions like 
#' [is.numeric] to select variables based on their properties.
#' 
#' @return 
#' An object of the same type as `.data`. Each object in the column `.nest_data` 
#' will also be of the same type as the input. Each object in `.nest_data` has
#' the following properties:
#' 
#' * Rows are not affect.
#' * Output columns are a subset of input columns, potentially with a different
#'   order. Columns will be renamed if `new_name = old_name` form is used.
#' * Data frame attributes are preserved.
#' * Groups are maintained; you can't select off grouping variables.
#' 
#' @details 
#' `nest_select()` is largely a wrapper for [dplyr::select()] and maintains the 
#' functionality of `select()` within each nested data frame. For more 
#' information on `select()`, please refer to the documentation in 
#' [`dplyr`](https://dplyr.tidyverse.org/).
#' 
#' @param .data A data frame, data frame extension (e.g., a tibble), or a lazy
#'   data frame (e.g., from dbplyr or dtplyr).
#' @param .nest_data A list-column containing data frames
#' @param ... One or more unquoted expressions separated by commas. Variable 
#'   names can be used if they were positions in the data frame, so expressions
#'   like `x:y` can be used to select a range of variables. 
#' 
#' @importFrom dplyr enquos
#' @importFrom dplyr mutate
#' @importFrom rlang :=
#' @importFrom purrr map
#' @importFrom dplyr select
#' 
#' @export
#' 
#' @family single table verbs
#' 
#' @examples 
#' gm_nest <- gapminder::gapminder %>% tidyr::nest(country_data = -continent)
#' 
#' gm_nest %>% nest_select(country_data, country, year, pop)
#' gm_nest %>% nest_select(country_data, where(is.numeric))
nest_select <- function(.data, 
                        .nest_data,
                        ...) {
  
  # assertions and checks
  check_nest_data(.data, {{ .nest_data }})
  
  # tidyeval of dots
  dots <- dplyr::enquos(...)
  
  # map select over list-col
  dplyr::mutate(
    .data,
    "{{.nest_data}}" := purrr::map({{ .nest_data }}, ~dplyr::select(.x, !!!dots))
  )
  
}