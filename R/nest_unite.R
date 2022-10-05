#' Unite multiple columns into one in a column of nested data frames
#' 
#' @description
#' `nest_unite()` is used to combine multiple columns into one in a column of
#' nested data frames.
#' 
#' @return
#'  An object of the same type as `.data`. Each object in the column `.nest_data` 
#'  will have a new column created as a combination of existing columns.
#'  
#' @details
#'  `nest_unite()` is a wrapper for [tidyr::unite()] and maintains the functionality 
#'  of `unite()` within each nested data frame. For more information on `unite()` 
#'  please refer to the documentation in ['tidyr'](https::/tidyr.tidyverse.org).
#'  
#' @inheritParams nest_select
#' @inheritParams tidyr::unite
#' @param ... Columns to unite.
#' 
#' @importFrom dplyr enquos
#' @importFrom dplyr mutate
#' @importFrom rlang :=
#' @importFrom purrr map
#' @importFrom tidyr unite
#' 
#' @export
#' @family tidyr verbs
#' 
#' @examples
#' set.seed(123)
#' gm <- gapminder::gapminder 
#' gm_nest <- gm %>% tidyr::nest(country_data = -continent)
#' 
#' gm_nest %>% 
#'   nest_unite(.nest_data = country_data, 
#'              col = comb, 
#'              year, 
#'              pop)
nest_unite <- function(.data,
                         .nest_data,
                         col,
                         ...,
                         sep = "_",
                         remove = TRUE,
                         na.rm = FALSE){
  
  # assertions and checks
  check_nest_data(.data, {{ .nest_data }})
  
  # tidyeval of dots
  dots <- dplyr::enquos(...)
  
  # map mutate over list-col
  dplyr::mutate(
    .data,
    "{{.nest_data}}" := purrr::map({{ .nest_data }}, 
                                   ~tidyr::unite(.x, col = {{ col }}, !!!dots, sep = sep,
                                                 remove = remove, na.rm = na.rm))
  )
}