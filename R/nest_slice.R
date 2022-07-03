#' @importFrom dplyr enquos
#' @importFrom dplyr mutate
#' @importFrom rlang :=
#' @importFrom purrr map
#' @importFrom dplyr slice
#' 
#' @export
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
