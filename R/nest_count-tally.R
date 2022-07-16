#' Count observations in a nested data frame by group
#' 
#' @description 
#' `nest_count()` lets you quickly count the unique values of one or more 
#' variables within each nested data frame. `nest_count()` results in a summary 
#' with one row per each set of variables to count by. `nest_add_count()` is 
#' equivalent with the exception that it retains all rows and adds a new column
#' with group-wise counts.
#' 
#' @return 
#' An object of the same type as `.data`. Each object in the column `.nest_data` 
#' will also be of the same type as the input. `nest_count()` and 
#' `nest_add_count()` group each object in `.nest_data` transiently, so the 
#' output returned in `.nest_data` will have the same groups as the input. 
#' 
#' @details 
#' `nest_count()` and `nest_add_count()` are largely wrappers for 
#' [dplyr::count()] and [dplyr::add_count()] and maintain the functionality of 
#' `count()` and `add_count()` within each nested data frame. For more 
#' information on `count()` and `add_count()`, please refer to the documentation
#' in [`dplyr`](https://dplyr.tidyverse.org/). 
#' 
#' @inheritParams nest_select
#' @param ... Variables to group by.
#' @param wt Frequency weights. 
#'   Can be `NULL` or a variable:
#'   
#'   * If `NULL` (the default), counts the number of rows in each group.
#'   * If a variable, computes `sum(wt)` for each group.
#' @param sort If `TRUE`, will show the largest groups at the top.
#' @param name The name of the new column in the output. 
#' 
#' @importFrom dplyr enquos
#' @importFrom dplyr mutate
#' @importFrom rlang :=
#' @importFrom purrr map
#' @importFrom dplyr count
#' 
#' @export
#' 
#' @examples 
#' gm_nest <- gapminder::gapminder %>% tidyr::nest(country_data = -continent)
#' 
#' # count the number of times each country appears in each nested tibble
#' gm_nest %>% nest_count(country_data, country)
#' gm_nest %>% nest_add_count(country_data, country)
#' 
#' # count the sum of population for each country in each nested tibble
#' gm_nest %>% nest_count(country_data, country, wt = pop)
#' gm_nest %>% nest_add_count(country_data, country, wt = pop)
nest_count <- function(.data,
                       .nest_data,
                       ...,
                       wt = NULL,
                       sort = FALSE,
                       name = NULL) {
  
  # assertions and checks
  check_nest_data(.data, {{ .nest_data }})
  
  # tidyeval of dots 
  dots <- dplyr::enquos(...)
  
  # map count over list-col
  dplyr::mutate(
    .data,
    "{{.nest_data}}" := purrr::map({{ .nest_data }}, ~dplyr::count(.x, !!!dots, wt = {{ wt }}, sort = sort, name = name))
  )
  
}

#' @importFrom dplyr enquos
#' @importFrom dplyr mutate
#' @importFrom rlang :=
#' @importFrom purrr map
#' @importFrom dplyr add_count
#' 
#' @export
#' @rdname nest_count
nest_add_count <- function(.data,
                           .nest_data,
                           ...,
                           wt = NULL,
                           sort = FALSE,
                           name = NULL) {
  
  # assertions and checks
  check_nest_data(.data, {{ .nest_data }})
  
  # tidyeval of dots
  dots <- dplyr::enquos(...)
  
  # map add_count over list-col
  dplyr::mutate(
    .data,
    "{{.nest_data}}" := purrr::map({{ .nest_data }}, ~dplyr::add_count(.x, !!!dots, wt = {{ wt }}, sort = sort, name = name))
  )
  
}
