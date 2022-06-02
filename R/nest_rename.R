#' @export
nest_rename <- function(.data, 
                        .nest_data,
                        ...) {
  
  # tidyeval of dots
  dots <- dplyr::enquos(...)
  
  # map rename over list-col
  dplyr::mutate(
    .data,
    "{{.nest_data}}" := purrr::map({{ .nest_data }}, ~dplyr::rename(.x, !!!dots))
  )
  
}

#' @export
nest_rename_with <- function(.data,
                             .nest_data,
                             .fn,
                             .cols = dplyr::everything(),
                             ...) {
  
  # tidyeval of dots
  dots <- dplyr::enquos(...)
  
  # map rename_with over list-col
  dplyr::mutate(
    .data,
    "{{.nest_data}}" := purrr::map({{ .nest_data }}, ~dplyr::rename_with(.x, .fn = .fn, .cols = .cols, !!!dots))
  )
  
}