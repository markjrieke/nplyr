#' @export
nest_distinct <- function(.data,
                          .nest_data,
                          ...,
                          .keep_all = FALSE) {
  
  # tidyeval of dots
  dots <- dplyr::enquos(...)
  
  # map distinct over list-col
  dplyr::mutate(
    .data,
    "{{.nest_data}}" := purrr::map({{ .nest_data }}, ~dplyr::distinct(.x, !!!dots, .keep_all = .keep_all))
  )
  
}


