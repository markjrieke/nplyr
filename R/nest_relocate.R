#' Change column order within a nested data frame
#' 
#' @description 
#' `nest_relocate()` changes column positions within a nested data frame, using 
#' the same syntax as [nest_select()] or [dplyr::select()] to make it easy to 
#' move blocks of columns at once.
#' 
#' @return 
#' An object of the same type as `.data`. Each object in the column `.nest_data` 
#' will also be of the same type as the input. Each object in `.nest_data` has
#' the following properties:
#' 
#' * Rows are not affected.
#' * The same columns appear in the output, but (usually) in a different place.
#' * Data frame attributes are preserved.
#' * Groups are not affected.
#' 
#' @details 
#' `nest_relocate()` is largely a wrapper for [dplyr::relocate()] and maintains 
#' the functionality of `relocate()` within each nested data frame. For more
#' information on `relocate()`, please refer to the documentation in
#' [`dplyr`](https://dplyr.tidyverse.org/).
#' 
#' @inheritParams nest_select
#' @param ... Columns to move.
#' @param .before,.after Destination of columns selected by `...`. Supplying 
#'   neither will move columns to the left-hand side; specifying both is an 
#'   error. 
#' 
#' @importFrom dplyr enquos
#' @importFrom dplyr mutate
#' @importFrom rlang :=
#' @importFrom purrr map
#' @importFrom dplyr relocate
#' 
#' @export
#' 
#' @examples 
#' gm_nest <- gapminder::gapminder %>% tidyr::nest(country_data = -continent)
#' 
#' gm_nest %>% nest_relocate(country_data, year)
#' gm_nest %>% nest_relocate(country_data, pop, .after = year)
nest_relocate <- function(.data, 
                          .nest_data,
                          ...,
                          .before = NULL,
                          .after = NULL) {
  
  # assertions and checks
  check_nest_data(.data, {{ .nest_data }})
  
  # tidyeval of dots
  dots <- dplyr::enquos(...)
  
  # map relocate over list-col
  dplyr::mutate(
    .data,
    "{{.nest_data}}" := purrr::map({{ .nest_data }}, ~dplyr::relocate(.x, !!!dots, .before = {{ .before }}, .after = {{ .after }}))
  )
  
}