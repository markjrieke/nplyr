#' @export
nest_summarise <- function(.data, 
                           .nest_data,
                           ...,
                           .groups = NULL) {
  
  # tidyeval of dots
  dots <- dplyr::enquos(...)
  
  # map summarise over list-col
  dplyr::mutate(
    .data,
    "{{.nest_data}}" := purrr::map({{ .nest_data }}, ~dplyr::summarise(.x, !!!dots, .groups = .groups))
  )
  
}

#' @export
nest_summarize <- nest_summarise

