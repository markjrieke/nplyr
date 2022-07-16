#' Subset rows in nested data frames using their positions.
#' 
#' @description 
#' `nest_slice()` lets you index rows in nested data frames by their (integer) 
#' locations. It allows you to select, remove, and duplicate rows. It is 
#' accompanied by a number of helpers for common use cases:
#' 
#' * `nest_slice_head()` and `nest_slice_tail()` select the first or last rows 
#'   of each nested data frame in `.nest_data`.
#' * `nest_slice_sample()` randomly selects rows from each data frame in 
#'   `.nest_data`.
#' * `nest_slice_min()` and `nest_slice_max()` select the rows with the highest 
#'   or lowest values of a variable within each nested data frame in 
#'   `.nest_data`.
#'   
#' If `.nest_data` is a grouped data frame, the operation will be performed on 
#' each group, so that (e.g.) `nest_slice_head(df, nested_dfs, n = 5)` will 
#' return the first five rows in each group for each nested data frame.
#' 
#' `nest_slice()` and its helpers are largely wrappers for [dplyr::slice()] and 
#' its helpers and maintains the functionality of `slice()` and its helpers 
#' within each nested data frame. For more information on `slice()` or its 
#' helpers, please refer to the documentation in 
#' [`dplyr`](https://dplyr.tidyverse.org/).
#' 
#' @inheritParams nest_select
#' @param ... For `nest_slice()`: Integer row values. 
#' 
#'   Provide either positive values to keep, or negative values to drop. The 
#'   values provided must be either all positive or all negative. Indices beyond
#'   the number of rows in the input are silently ignored.
#'   
#'   For `nest_slice_helpers()`, these arguments are passed on to methods.
#'   
#'   Additionally:
#'   
#'   * `n`,`prop` Provide either `n`, the number of rows, or `prop`, the
#'     proportion of rows to select. If neither are supplied, `n = 1` will be
#'     used.
#'     
#'     If a negative value of `n` or `prop` is provided, the specified number or 
#'     proportion of rows will be removed.
#'   
#'     If `n` is greater than the number of rows in the group (or `prop > 1`), the
#'     result will be silently truncated to the group size. If the proportion of a 
#'     group size does not yield an integer number of rows, the absolute value of 
#'     `prop*nrow(.nest_data)` is rounded down.
#' 
#' @param .preserve Relevant when `.nest_data` is grouped. 
#'   If `.preserve = FALSE` (the default), the grouping structure is 
#'   recalculated based on the resulting data, otherwise the grouping data is 
#'   kept as is.
#' 
#' @importFrom dplyr enquos
#' @importFrom dplyr mutate
#' @importFrom rlang :=
#' @importFrom purrr map
#' @importFrom dplyr slice
#' 
#' @export
#' 
#' @family single table verbs
#' 
#' @examples 
#' gm_nest <- gapminder::gapminder %>% tidyr::nest(country_data = -continent)
#' 
#' # select the 1st, 3rd, and 5th rows in each data frame in country_data
#' gm_nest %>% nest_slice(country_data, 1, 3, 5)
#' 
#' # or select all but the 1st, 3rd, and 5th rows:
#' gm_nest %>% nest_slice(country_data, -1, -3, -5)
#' 
#' # first and last rows based on existing order:
#' gm_nest %>% nest_slice_head(country_data, n = 5)
#' gm_nest %>% nest_slice_tail(country_data, n = 5)
#' 
#' # rows with minimum and maximum values of a variable:
#' gm_nest %>% nest_slice_min(country_data, lifeExp, n = 5)
#' gm_nest %>% nest_slice_max(country_data, lifeExp, n = 5)
#' 
#' # randomly select rows with or without replacement:
#' gm_nest %>% nest_slice_sample(country_data, n = 5)
#' gm_nest %>% nest_slice_sample(country_data, n = 5, replace = TRUE)
nest_slice <- function(.data, 
                       .nest_data,
                       ...,
                       .preserve = FALSE) {
  
  # assertions and checks
  check_nest_data(.data, {{ .nest_data }})
  
  # tidyeval of dots
  dots <- dplyr::enquos(...)
  
  # map slice over list-col
  dplyr::mutate(
    .data,
    "{{.nest_data}}" := purrr::map({{ .nest_data }}, ~dplyr::slice(.x, !!!dots, .preserve = .preserve))
  )
  
}

#' @importFrom dplyr enquos
#' @importFrom dplyr mutate
#' @importFrom rlang :=
#' @importFrom purrr map
#' @importFrom dplyr slice_head
#' 
#' @export
#' @rdname nest_slice
nest_slice_head <- function(.data,
                            .nest_data,
                            ...) {
  
  # assertions and checks
  check_nest_data(.data, {{ .nest_data }})
  
  # tidyeval of dots
  dots <- dplyr::enquos(...)
  
  # map slice_head over list-col
  dplyr::mutate(
    .data,
    "{{.nest_data}}" := purrr::map({{ .nest_data }}, ~dplyr::slice_head(.x, !!!dots))
  )
  
}

#' @importFrom dplyr enquos
#' @importFrom dplyr mutate
#' @importFrom rlang :=
#' @importFrom purrr map
#' @importFrom dplyr slice_tail
#' 
#' @export
#' @rdname nest_slice
nest_slice_tail <- function(.data,
                            .nest_data,
                            ...) {
  
  # assertions and checks
  check_nest_data(.data, {{ .nest_data }})
  
  # tidyeval of dots
  dots <- dplyr::enquos(...)
  
  # map slice_tail over list-col
  dplyr::mutate(
    .data,
    "{{.nest_data}}" := purrr::map({{ .nest_data }}, ~dplyr::slice_tail(.x, !!!dots))
  )
  
}

#' @importFrom dplyr enquos
#' @importFrom dplyr mutate
#' @importFrom rlang :=
#' @importFrom purrr map
#' @importFrom dplyr slice_min
#' 
#' @export
#' @rdname nest_slice
#' 
#' @param order_by Variable or function of variables to order by.
#' @param with_ties Should ties be kept together? The default, `TRUE`, may 
#'   return more rows than you request. Use `FALSE` to ignore ties and return 
#'   the first `n` rows. 
nest_slice_min <- function(.data,
                           .nest_data,
                           order_by, 
                           ...,
                           with_ties = TRUE) {
  
  # assertions and checks
  check_nest_data(.data, {{ .nest_data }})
  
  # tidyeval of dots 
  dots <- dplyr::enquos(...)
  
  # map slice_min over list-col
  dplyr::mutate(
    .data,
    "{{.nest_data}}" := purrr::map({{ .nest_data }}, ~dplyr::slice_min(.x, order_by = {{ order_by }}, !!!dots, with_ties = with_ties))
  )
  
}

#' @importFrom dplyr enquos
#' @importFrom dplyr mutate
#' @importFrom rlang :=
#' @importFrom purrr map
#' @importFrom dplyr slice_max
#' 
#' @export
#' @rdname nest_slice
nest_slice_max <- function(.data,
                           .nest_data,
                           order_by,
                           ...,
                           with_ties = TRUE) {
  
  # assertions and checks
  check_nest_data(.data, {{ .nest_data }})
  
  # tidyeval of dots
  dots <- dplyr::enquos(...) 
  
  # map slice_max over list-col
  dplyr::mutate(
    .data,
    "{{.nest_data}}" := purrr::map({{ .nest_data }}, ~dplyr::slice_max(.x, order_by = {{ order_by }}, !!!dots, with_ties = with_ties))
  )
  
}

#' @importFrom dplyr enquos
#' @importFrom dplyr mutate
#' @importFrom rlang :=
#' @importFrom purrr map
#' @importFrom dplyr slice_sample
#' 
#' @export
#' @rdname nest_slice
#' 
#' @param replace Should sampling be performed with (`TRUE`) or without (`FALSE`,
#'   the default) replacement?
#' @param weight_by Sampling weights. This must evaluate to a vector of 
#'   non-negative numbers the same length as the input. Weights are automatically
#'   standardised to sum to 1.
nest_slice_sample <- function(.data,
                              .nest_data,
                              ...,
                              weight_by = NULL,
                              replace = FALSE) {
  
  # assertions and checks
  check_nest_data(.data, {{ .nest_data }})
  
  # tidyeval of dots
  dots <- dplyr::enquos(...)
  
  # map slice_sample over list-col
  dplyr::mutate(
    .data,
    "{{.nest_data}}" := purrr::map({{ .nest_data }}, ~dplyr::slice_sample(.x, !!!dots, weight_by = weight_by, replace = replace))
  )
  
}
