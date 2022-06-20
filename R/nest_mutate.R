#' @importFrom dplyr enquos
#' @importFrom dplyr mutate
#' @importFrom rlang :=
#' @importFrom purrr map
#' 
#' @export
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