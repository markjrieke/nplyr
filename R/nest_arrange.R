#' @export
nest_arrange <- function(.data, 
                         .nest_data,
                         ...,
                         .by_group = FALSE) {
  
  # tidyeval of dots
  dots <- dplyr::enquos(...)
  
  # map arrange over list-col
  dplyr::mutate(
    .data,
    "{{.nest_data}}" := purrr::map({{ .nest_data }}, ~dplyr::arrange(.x, !!!dots, .by_group = .by_group))
  )
  
}
