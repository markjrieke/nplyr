#' Create, modify, and delete columns in nested data frames
#' 
#' `nest_mutate()` adds new variables to and preserves existing ones within 
#' the nested data frames in `.nest_data`.
#' `nest_transmute()` adds new variables to and drops existing ones from the 
#' nested data frames in `.nest_data`. 
#' 
#' `nest_mutate()` and `nest_transmute()` are largely wrappers for 
#' [dplyr::mutate()] and [dplyr::transmute()] and maintain the functionality of 
#' `mutate()` and `transmute()` within each nested data frame. For more 
#' information on `mutate()` or `transmute()`, please refer to the documentation
#' in [`dplyr`](https://dplyr.tidyverse.org/).
#' 
#' @inheritParams generic-params
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
#' \dontrun{
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
#' }
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