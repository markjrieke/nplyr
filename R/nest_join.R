#' Nested Mutating joins
#' 
#' @description 
#' Nested mutating joins add columns from `y` to each of the nested data frames
#' in `.nest_data`, matching observations based on the keys. There are four 
#' nested mutating joins: 
#' 
#' ## Inner join
#' 
#' `nest_inner_join()` only keeps observations from `.nest_data` that have a 
#' matching key in `y`.
#' 
#' The most important property of an inner join is that unmatched rows in either
#' input are not included in the result.
#' 
#' ## Outer joins
#' 
#' There are three outer joins that keep observations that appear in at least 
#' one of the data frames:
#' 
#' * `nest_left_join()` keeps all observations in `.nest_data`.
#' * `nest_right_join()` keeps all observations in `y`.
#' * `nest_full_join()` keeps all observations in `.nest_data` and `y`. 
#' 
#' @return 
#' An object of the same type as `.data`. Each object in the column `.nest_data` 
#' will also be of the same type as the input. The order of the rows and columns 
#' of each object in `.nest_data` is preserved as much as possible. Each object
#' in `.nest_data` has the following properties:
#' 
#' * For `nest_inner_join()`, a subset of rows in each object in `.nest_data`.
#'   For `nest_left_join()`, all rows in each object in `.nest_data`.
#'   For `nest_right_join()`, a subset of rows in each object in `.nest_data`, 
#'   followed by unmatched `y` rows.
#'   For `nest_full_join()`, all rows in each object in `.nest_data`, followed 
#'   by unmatched `y` rows.
#' * Output columns include all columns from each `.nest_data` and all non-key 
#'   columns from `y`. If `keep = TRUE`, the key columns from `y` are included
#'   as well.
#' * If non-key columns in any object in `.nest_data` and `y` have the same name,
#'   `suffix`es are added to disambiguate. If `keep = TRUE` and key columns in 
#'   `.nest_data` and `y` have the same name, `suffix`es are added to 
#'   disambiguate these as well.
#' * If `keep = FALSE`, output columns included in `by` are coerced to their 
#'   common type between the objects in `.nest_data` and `y`. 
#' * Groups are taken from `.nest_data`.
#' 
#' @details 
#' `nest_inner_join()`, `nest_left_join()`, `nest_right_join()`, and 
#' `nest_full_join()` are largely wrappers for [dplyr::inner_join()], 
#' [dplyr::left_join()], [dplyr::right_join()], and [dplyr::full_join()] and 
#' maintain the functionality of these verbs within each nested data frame. For 
#' more information on `inner_join()`, `left_join()`, `right_join()`, or 
#' `full_join()`, please refer to the documentation in 
#' [`dplyr`](https://dplyr.tidyverse.org/).
#' 
#' @inheritParams nest_select
#' @param y A data frame, data frame extension (e.g., a tibble), or a lazy data
#'   frame (e.g., from dbplyr or dtplyr).
#' @param by A character vector of variables to join by or a join specification 
#'   created with `join_by()`.
#'   
#'   If `NULL`, the default, `nest_*_join()` will perform a natural join, using 
#'   all variables in common across each object in `.nest_data` and `y`. A 
#'   message lists the variables so you can check they're correct; suppress the 
#'   message by supplying `by` explicitly.
#'   
#'   To join on different variables between the objects in `.nest_data` and `y`,
#'   use a named vector. For example, `by = c("a" = "b")` will match 
#'   `.nest_data$a` to `y$b` for each object in `.nest_data`. 
#'   
#'   To join by multiple variables, use a vector with length >1. For example, 
#'   `by = c("a", "b")` will match `.nest_data$a` to `y$a` and `.nest_data$b` to 
#'   `y$b` for each object in `.nest_data`. Use a named vector to match 
#'   different variables in `.nest_data` and `y`. For example,
#'   `by = c("a" = "b", "c" = "d")` will match `.nest_data$a` to `y$b` and 
#'   `.nest_data$c` to `y$d` for each object in `.nest_data`.
#'   
#'   To perform a cross-join, generating all combinations of each object in 
#'   `.nest_data` and `y`, use `by = character()`.
#' @param copy If `.nest_data` and `y` are not from the same data source and 
#'   `copy = TRUE` then `y` will be copied into the same src as `.nest_data`. 
#'   *(Need to review this parameter in more detail for applicability with nplyr)* 
#' @param suffix If there are non-joined duplicate variables in `.nest_data` and 
#'   `y`, these suffixes will be added to the output to disambiguate them. 
#'   Should be a character vector of length 2.
#' @param keep Should the join keys from both `.nest_data` and `y` be preserved 
#'   in the output?
#' @param ... Other parameters passed onto methods. Includes:
#' 
#'   * `na_matches` : Should two `NA` or two `NaN` values match?
#'   
#'     - `"na"`, the default, treats two `NA` or two `NaN` values as equal.
#'     - `"never"` treats two `NA` or two `NaN` values as different, and will
#'       never match them together or to any other values.
#'   
#'   * `multiple` : Handling of rows in `.nest_data` with multiple matches in `y`.
#'   
#'     - `"all"` returns every match detected in `y`.
#'     - `"any"` returns one match detected in `y`, with no guarantees on which
#'       match will be returned. It is often faster than `"first"` and `"last"` if
#'       you just need to detect if there is at least one match.
#'     - `"first"` returns the first match detected in `y`.
#'     - `"last"` returns the last match detected in `y`.
#'     - `"warning"` throws a warning if multiple matches are detected, and then
#'       falls back to `"all"`.
#'     - `"error"` throws an error if multiple matches are detected.
#'     
#'  * `unmatched` : How should unmatched keys that would result in dropped rows
#'    be handled?
#'    
#'    - `"drop"` drops unmatched keys from the result.
#'    - `"error"` throws an error if unmatched keys are detected.
#'   
#' @family joins
#' 
#' @examples 
#' gm_nest <- gapminder::gapminder %>% tidyr::nest(country_data = -continent)
#' gm_codes <- gapminder::country_codes
#' 
#' gm_nest %>% nest_inner_join(country_data, gm_codes, by = "country")
#' gm_nest %>% nest_left_join(country_data, gm_codes, by = "country")
#' gm_nest %>% nest_right_join(country_data, gm_codes, by = "country")
#' gm_nest %>% nest_full_join(country_data, gm_codes, by = "country")
#' 
#' @name nest-mutate-joins
NULL

#' @importFrom dplyr enquos
#' @importFrom dplyr mutate
#' @importFrom rlang :=
#' @importFrom purrr map
#' @importFrom dplyr inner_join
#' 
#' @export
#' @rdname nest-mutate-joins
nest_inner_join <- function(.data,
                            .nest_data,
                            y,
                            by = NULL,
                            copy = FALSE,
                            suffix = c(".x", ".y"),
                            ...,
                            keep = FALSE) {
  
  # assertions and checks
  check_nest_data(.data, {{ .nest_data }})
  
  # tidyeval of dots
  dots <- dplyr::enquos(...)
  
  # map inner_join over list-col
  dplyr::mutate(
    .data,
    "{{.nest_data}}" := purrr::map({{ .nest_data }}, ~dplyr::inner_join(.x, y = y, by = by, copy = copy, suffix = suffix, !!!dots, keep = keep))
  )
  
}

#' @importFrom dplyr enquos
#' @importFrom dplyr mutate
#' @importFrom rlang :=
#' @importFrom purrr map
#' @importFrom dplyr left_join
#' 
#' @export
#' @rdname nest-mutate-joins
nest_left_join <- function(.data,
                           .nest_data,
                           y,
                           by = NULL,
                           copy = FALSE,
                           suffix = c(".x", ".y"),
                           ...,
                           keep = FALSE) {
  
  # assertions and checks
  check_nest_data(.data, {{ .nest_data }})
  
  # tidyeval of dots
  dots <- dplyr::enquos(...)
  
  # map left_join over list-col
  dplyr::mutate(
    .data,
    "{{.nest_data}}" := purrr::map({{ .nest_data }}, ~dplyr::left_join(.x, y = y, by = by, copy = copy, suffix = suffix, !!!dots, keep = keep))
  )
  
}

#' @importFrom dplyr enquos
#' @importFrom dplyr mutate
#' @importFrom rlang :=
#' @importFrom purrr map
#' @importFrom dplyr right_join
#' 
#' @export
#' @rdname nest-mutate-joins
nest_right_join <- function(.data,
                            .nest_data,
                            y,
                            by = NULL,
                            copy = FALSE,
                            suffix = c(".x", ".y"),
                            ...,
                            keep = FALSE) {
  
  # assertions and checks
  check_nest_data(.data, {{ .nest_data }})
  
  # tidyeval of dots
  dots <- dplyr::enquos(...)
  
  # map right_join over list-col
  dplyr::mutate(
    .data,
    "{{.nest_data}}" := purrr::map({{ .nest_data }}, ~dplyr::right_join(.x, y = y, by = by, copy = copy, suffix = suffix, !!!dots, keep = keep))
  )
  
}

#' @importFrom dplyr enquos
#' @importFrom dplyr mutate
#' @importFrom rlang :=
#' @importFrom purrr map
#' @importFrom dplyr full_join
#' 
#' @export
#' @rdname nest-mutate-joins
nest_full_join <- function(.data,
                           .nest_data,
                           y,
                           by = NULL,
                           copy = FALSE,
                           suffix = c(".x", ".y"),
                           ...,
                           keep = FALSE) {
  
  # assertions and checks
  check_nest_data(.data, {{ .nest_data }})
  
  # tidyeval of dots
  dots <- dplyr::enquos(...)
  
  # map full_join over list-col
  dplyr::mutate(
    .data,
    "{{.nest_data}}" := purrr::map({{ .nest_data }}, ~dplyr::full_join(.x, y = y, by = by, copy = copy, suffix = suffix, !!!dots, keep = keep))
  )
  
}

#' Nested filtering joins
#' 
#' @description 
#' Nested filtering joins filter rows from `.nest_data` based on the presence or 
#' absence of matches in `y`:
#' 
#' * `nest_semi_join()` returns all rows from `.nest_data` with a match in `y`.
#' * `nest_anti_join()` returns all rows from `.nest_data` with*out* a match in `y`.
#' 
#' @return 
#' An object of the same type as `.data`. Each object in the column `.nest_data` 
#' will also be of the same type as the input. Each object in `.nest_data` has
#' the following properties:
#' 
#' * Rows are a subset of the input, but appear in the same order.
#' * Columns are not modified.
#' * Data frame attributes are preserved.
#' * Groups are taken from `.nest_data`. The number of groups may be reduced.
#' 
#' @details 
#' `nest_semi_join()` and `nest_anti_join()` are largely wrappers for 
#' [dplyr::semi_join()] and [dplyr::anti_join()] and maintain the functionality 
#' of `semi_join()` and `anti_join()` within each nested data frame. For more 
#' information on `semi_join()` or `anti_join()`, please refer to the 
#' documentation in [`dplyr`](https://dplyr.tidyverse.org/).
#' 
#' @inheritParams nest_select
#' @inheritParams nest_left_join
#' 
#' @family joins
#' 
#' @examples 
#' gm_nest <- gapminder::gapminder %>% tidyr::nest(country_data = -continent)
#' gm_codes <- gapminder::country_codes %>% dplyr::slice_sample(n = 10)
#' 
#' gm_nest %>% nest_semi_join(country_data, gm_codes, by = "country")
#' gm_nest %>% nest_anti_join(country_data, gm_codes, by = "country")
#' 
#' @name nest-filter-joins
NULL

#' @importFrom dplyr enquos
#' @importFrom dplyr mutate
#' @importFrom rlang :=
#' @importFrom purrr map
#' @importFrom dplyr semi_join
#' 
#' @export
#' @rdname nest-filter-joins
nest_semi_join <- function(.data,
                           .nest_data,
                           y,
                           by = NULL,
                           copy = FALSE,
                           ...) {
  
  # assertions and checks
  check_nest_data(.data, {{ .nest_data }})
  
  # tidyeval of dots 
  dots <- dplyr::enquos(...)
  
  # map semi_join over list-col
  dplyr::mutate(
    .data,
    "{{.nest_data}}" := purrr::map({{ .nest_data }}, ~dplyr::semi_join(.x, y = y, by = by, copy = copy, !!!dots))
  )
  
}

#' @importFrom dplyr enquos
#' @importFrom dplyr mutate
#' @importFrom rlang :=
#' @importFrom purrr map
#' @importFrom dplyr anti_join
#' 
#' @export
#' @rdname nest-filter-joins
nest_anti_join <- function(.data,
                           .nest_data,
                           y,
                           by = NULL,
                           copy = FALSE,
                           ...) {
  
  # assertions and checks
  check_nest_data(.data, {{ .nest_data }})
  
  # tidyeval of dots
  dots <- dplyr::enquos(...)
  
  # map anti_join over list-col
  dplyr::mutate(
    .data,
    "{{.nest_data}}" := purrr::map({{ .nest_data }}, ~dplyr::anti_join(.x, y = y, by = by, copy = copy, !!!dots))
  )
  
}

#' Nested nest join
#' 
#' `nest_nest_join()` returns all rows and columns in `.nest_data` with a new 
#' nested-df column that contains all matches from `y`. When there is no match, 
#' the list contains a 0-row tibble.
#' 
#' @return 
#' An object of the same type as `.data`. Each object in the column `.nest_data` 
#' will also be of the same type as the input. 
#' 
#' @details 
#' `nest_nest_join()` is largely a wrapper around [dplyr::nest_join()] and 
#' maintains the functionality of `nest_join()` within east nested data frame. 
#' For more information on `nest_join()`, please refer to the documentation in
#' [`dplyr`](https://dplyr.tidyverse.org/).
#' 
#' @inheritParams nest_select
#' @inheritParams nest_left_join
#' @param name The name of the list column nesting joins create. If `NULL`, the 
#'   name of `y` is used.
#' 
#' @importFrom dplyr enquos
#' @importFrom dplyr mutate
#' @importFrom rlang :=
#' @importFrom purrr map
#' @importFrom dplyr nest_join
#' 
#' @export
#' 
#' @family joins
#' 
#' @examples 
#' gm_nest <- gapminder::gapminder %>% tidyr::nest(country_data = -continent)
#' gm_codes <- gapminder::country_codes
#' 
#' gm_nest %>% nest_nest_join(country_data, gm_codes, by = "country")
nest_nest_join <- function(.data,
                           .nest_data,
                           y,
                           by = NULL,
                           copy = FALSE,
                           keep = FALSE,
                           name = NULL,
                           ...) {
  
  # assertions and checks
  check_nest_data(.data, {{ .nest_data }})
  
  # tidyeval of dots
  dots <- dplyr::enquos(...)
  
  # map nest_join over list-col
  dplyr::mutate(
    .data,
    "{{.nest_data}}" := purrr::map({{ .nest_data }}, ~dplyr::nest_join(.x, y = y, by = by, copy = copy, keep = keep, name = name, !!!dots))
  )
  
}
