#' @export
nest_relocate <- function(.data, 
                          .nest_data,
                          ...,
                          .before = NULL,
                          .after = NULL) {
  
  # tidyeval of dots
  dots <- dplyr::enquos(...)
  
  # map relocate over list-col
  dplyr::mutate(
    .data,
    "{{.nest_data}}" := purrr::map({{ .nest_data }}, ~dplyr::relocate(.x, !!!dots, .before = .before, .after = .after))
  )
  
}