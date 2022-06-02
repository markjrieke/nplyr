#' @export
nest_group_by <- function(.data,
                          .nest_data,
                          ...,
                          .add = FALSE,
                          .drop = TRUE) {
  
  # tidyeval of dots
  dots <- dplyr::enquos(...)
  
  # map group_by over list-col
  dplyr::mutate(
    .data,
    "{{.nest_data}}" := purrr::map({{ .nest_data }}, ~dplyr::group_by(.x, !!!dots, .add = .add, .drop = .drop))
  )
  
}

#' @export
nest_ungroup <- function(.data,
                         .nest_data,
                         ...) {
  
  # tidyeval of dots
  dots <- dplyr::enquos(...)
  
  # map ungroup over list-col
  dplyr::mutate(
    .data,
    "{{.nest_data}}" := purrr::map({{ .nest_data }}, ~dplyr::ungroup(.x, !!!dots))
  )
  
}

