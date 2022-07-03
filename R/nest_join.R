#' @importFrom dplyr enquos
#' @importFrom dplyr mutate
#' @importFrom rlang :=
#' @importFrom purrr map
#' @importFrom dplyr inner_join
#' 
#' @export
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

#' @importFrom dplyr enquos
#' @importFrom dplyr mutate
#' @importFrom rlang :=
#' @importFrom purrr map
#' @importFrom dplyr semi_join
#' 
#' @export
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

#' @importFrom dplyr enquos
#' @importFrom dplyr mutate
#' @importFrom rlang :=
#' @importFrom purrr map
#' @importFrom dplyr nest_join
#' 
#' @export
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
