#' Rename columns in nested data frames
#' 
#' @description 
#' `nest_rename()` changes the names of individual variables using 
#' `new_name = old_name` syntax; `nest_rename_with()` renames columns using a 
#' function.
#' 
#' @return 
#' An object of the same type as `.data`. Each object in the column `.nest_data` 
#' will also be of the same type as the input. Each object in `.nest_data` has
#' the following properties:
#' 
#' * Rows are not affected.
#' * Column names are changed; column order is preserved.
#' * Data frame attributes are preserved.
#' * Groups are updated to reflect new names.
#' 
#' @details 
#' `nest_rename()` and `nest_rename_with()` are largely wrappers for 
#' [dplyr::rename()] and [dplyr::rename_with()] and maintain the functionality 
#' of `rename()` and `rename_with()` within each nested data frame. For more 
#' information on `rename()` or `rename_with()`, please refer to the 
#' documentation in [`dplyr`](https://dplyr.tidyverse.org/).
#' 
#' @inheritParams nest_select
#' @param ...
#'   For `nest_rename()`: Use `new_name = old_name` to rename selected variables.
#'   
#'   For `nest_rename_with()`: additional arguments passed onto `.fn`. 
#' 
#' @importFrom dplyr enquos
#' @importFrom dplyr mutate
#' @importFrom rlang :=
#' @importFrom purrr map
#' @importFrom dplyr rename
#' 
#' @export
#' 
#' @family single table verbs
#' 
#' @examples 
#' gm_nest <- gapminder::gapminder %>% tidyr::nest(country_data = -continent)
#' 
#' gm_nest %>% nest_rename(country_data, population = pop)
#' gm_nest %>% nest_rename_with(country_data, stringr::str_to_lower)
nest_rename <- function(.data, 
                        .nest_data,
                        ...) {
  
  # assertions and checks
  check_nest_data(.data, {{ .nest_data }})
  
  # tidyeval of dots
  dots <- dplyr::enquos(...)
  
  # map rename over list-col
  dplyr::mutate(
    .data,
    "{{.nest_data}}" := purrr::map({{ .nest_data }}, ~dplyr::rename(.x, !!!dots))
  )
  
}

#' @importFrom dplyr enquos
#' @importFrom dplyr mutate
#' @importFrom rlang :=
#' @importFrom purrr map
#' @importFrom dplyr rename_with
#' @importFrom dplyr everything
#' 
#' @export
#' 
#' @param .fn A function used to transform the selected `.cols`. Should return a 
#'   character vector the same length as the input.
#' @param .cols Columns to rename; defaults to all columns. 
#' 
#' @rdname nest_rename
nest_rename_with <- function(.data,
                             .nest_data,
                             .fn,
                             .cols = dplyr::everything(),
                             ...) {
  
  # assertions and checks
  check_nest_data(.data, {{ .nest_data }})
  
  # tidyeval of dots
  dots <- dplyr::enquos(...)
  
  # map rename_with over list-col
  dplyr::mutate(
    .data,
    "{{.nest_data}}" := purrr::map({{ .nest_data }}, ~dplyr::rename_with(.x, .fn = .fn, .cols = .cols, !!!dots))
  )
  
}