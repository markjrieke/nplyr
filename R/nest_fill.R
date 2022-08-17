#' Fill missing values in a column of nested data frames
#' 
#' @description
#' `nest_fill()` is used to fill missing values in selected columns using the next 
#' or previous entries in a column of nested data frames.
#' 
#' @return
#'  An object of the same type as `.data`. Each object in the column `.nest_data` 
#'  will have the chosen columns filled in the direction specified by `.direction`.
#'  
#' @details
#'  `nest_fill()` is a wrapper for [tidyr::fill()] and maintains the functionality 
#'  of `fill()` within each nested data frame. For more information on `fill()` 
#'  please refer to the documentation in ['tidyr'](https::/tidyr.tidyverse.org).
#'  
#' @inheritParams nest_select
#' @param ... <[`tidy-select`][tidyr_tidy_select]> Columns to fill.
#' @param .direction Direction in which to fill missing values. Currently either 
#' "down" (the default), "up", "downup" (i.e. first down and then up) or "updown" 
#' (first up and then down).
#' 
#' @importFrom dplyr enquos
#' @importFrom dplyr mutate
#' @importFrom rlang :=
#' @importfrom purrr map
#' @importFrom tidyr fill
#' 
#' @export
#' @family single table verbs
#' 
#' @examples
#' set.seed(123)
#' gm <- gapminder::gapminder %>% mutate(pop = if_else(runif(n()) >= 0.9,NA_integer_,pop))
#' gm_nest <- gm %>% tidyr::nest(country_data = -continent)
#' 
#' gm_nest %>% nest_fill(.nest_data = country_data,pop,.direction = "down")
nest_fill <- function(.data,
                      .nest_data,
                      ...,
                      .direction = c("down","up","downup","updown")){
  
  # assertions and checks
  check_nest_data(.data, {{ .nest_data }})
  
  # tidyeval of dots
  dots <- dplyr::enquos(...)
  
  # map mutate over list-col
  dplyr::mutate(
    .data,
    "{{.nest_data}}" := purrr::map({{ .nest_data }}, ~tidyr::fill(.x, !!!dots, .direction = .direction))
  )
}