#' Drop rows containing missing values in a column of nested data frames
#' 
#' @description
#' `nest_drop_na()` is used to drop rows from each data frame in a column of
#' nested data frames.
#' 
#' @return
#'  An object of the same type as `.data`. Each object in the column `.nest_data` 
#'  will have rows dropped according to the presence of NAs.
#'  
#' @details
#'  `nest_drop_na()` is a wrapper for [tidyr::drop_na()] and maintains the functionality 
#'  of `drop_na()` within each nested data frame. For more information on `drop_na()` 
#'  please refer to the documentation in ['tidyr'](https::/tidyr.tidyverse.org).
#'  
#' @inheritParams nest_select
#' @param ... Columns within `.nest_data` to inspect for missing values. If empty,
#'   all columns within each dataframe in `.nest_data` are used.
#' 
#' @importFrom dplyr enquos
#' @importFrom dplyr mutate
#' @importFrom rlang :=
#' @importFrom purrr map
#' @importFrom tidyr drop_na
#' 
#' @export
#' @family tidyr verbs
#' 
#' @examples
#' gm <- gapminder::gapminder 
#' 
#' # randomly insert NAs into the dataframe & nest
#' set.seed(123) 
#' gm <- gm %>% mutate(pop = if_else(runif(nrow(gm)) >= 0.9, NA_integer_, pop))
#' gm_nest <- gm %>% tidyr::nest(country_data = -continent)
#' 
#' # drop rows where an NA exists in column `pop`
#' gm_nest %>% nest_drop_na(.nest_data = country_data,pop)
nest_drop_na <- function(.data,
                         .nest_data,
                         ...){
  
  # assertions and checks
  check_nest_data(.data, {{ .nest_data }})
  
  # tidyeval of dots
  dots <- dplyr::enquos(...)
  
  # map mutate over list-col
  dplyr::mutate(
    .data,
    "{{.nest_data}}" := purrr::map({{ .nest_data }}, 
                                   ~tidyr::drop_na(.x, !!!dots))
  )
}