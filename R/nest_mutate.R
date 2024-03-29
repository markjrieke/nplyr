#' Create, modify, and delete columns in nested data frames
#' 
#' @description 
#' `nest_mutate()` adds new variables to and preserves existing ones within 
#' the nested data frames in `.nest_data`.
#' `nest_transmute()` adds new variables to and drops existing ones from the 
#' nested data frames in `.nest_data`. 
#' 
#' @return 
#' An object of the same type as `.data`. Each object in the column `.nest_data` 
#' will also be of the same type as the input. Each object in `.nest_data` has
#' the following properties:
#' 
#' * For `nest_mutate()`:
#'   * Columns from each object in `.nest_data` will be preserved according to 
#'     the `.keep` argument. 
#'   * Existing columns that are modified by `...` will always be returned in 
#'     their original location.
#'   * New columns created through `...` will be placed according to the 
#'     `.before` and `.after` arguments.
#' * For `nest_transmute()`:
#'   * Columns created or modified through `...` will be returned in the order 
#'     specified by `...`.
#'   * Unmodified grouping columns will be placed at the front.
#' * The number of rows is not affected.
#' * Columns given the value `NULL` will be removed.
#' * Groups will be recomputed if a grouping variable is mutated.
#' * Data frame attributes will be preserved.
#' 
#' @details 
#' `nest_mutate()` and `nest_transmute()` are largely wrappers for 
#' [dplyr::mutate()] and [dplyr::transmute()] and maintain the functionality of 
#' `mutate()` and `transmute()` within each nested data frame. For more 
#' information on `mutate()` or `transmute()`, please refer to the documentation
#' in [`dplyr`](https://dplyr.tidyverse.org/).
#' 
#' @inheritParams nest_select
#' @param ... Name-value pairs.
#'   The name gives the name of the column in the output.
#'   
#'   The value can be:
#'   
#'   * A vector of length 1, which will be recycled to the correct length.
#'   * `NULL`, to remove the column.
#'   * A data frame or tibble, to create multiple columns in the output.
#' 
#' @importFrom dplyr enquos
#' @importFrom dplyr mutate
#' @importFrom rlang :=
#' @importFrom purrr map
#' 
#' @export
#' 
#' @family single table verbs
#' 
#' @examples 
#' gm_nest <- gapminder::gapminder %>% tidyr::nest(country_data = -continent)
#' 
#' # add or modify columns:
#' gm_nest %>%
#'   nest_mutate(
#'     country_data,
#'     lifeExp = NULL,
#'     gdp = gdpPercap * pop,
#'     pop = pop/1000000
#'   )
#'   
#' # use dplyr::across() to apply transformation to multiple columns 
#' gm_nest %>%
#'   nest_mutate(
#'     country_data,
#'     across(c(lifeExp:gdpPercap), mean)
#'   )
#' 
#' # nest_transmute() drops unused columns when mutating:
#' gm_nest %>%
#'   nest_transmute(
#'     country_data,
#'     country = country,
#'     year = year,
#'     pop = pop/1000000
#'   )
nest_mutate <- function(.data, 
                        .nest_data,
                        ...) {
  
  # assertions and checks
  check_nest_data(.data, {{ .nest_data }})
  
  # tidyeval of dots
  dots <- dplyr::enquos(...)
  
  # map mutate over list-col
  dplyr::mutate(
    .data,
    "{{.nest_data}}" := purrr::map({{ .nest_data }}, ~dplyr::mutate(.x, !!!dots))
  )
  
}

#' @importFrom dplyr enquos
#' @importFrom dplyr mutate
#' @importFrom rlang :=
#' @importFrom purrr map
#' @importFrom dplyr transmute
#' 
#' @export
#' @rdname nest_mutate
nest_transmute <- function(.data,
                           .nest_data,
                           ...) {
  
  # assertions and checks
  check_nest_data(.data, {{ .nest_data }})
  
  # tidyeval of dots
  dots <- dplyr::enquos(...)
  
  # map transmute over list-col
  dplyr::transmute(
    .data,
    "{{.nest_data}}" := purrr::map({{ .nest_data }}, ~dplyr::transmute(.x, !!!dots))
  )
  
}