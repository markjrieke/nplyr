#' @export
nest_slice <- function(.data, 
                       .nest_data,
                       ...,
                       .preserve = FALSE) {
  
  # tidyeval of dots
  dots <- dplyr::enquos(...)
  
  # map slice over list-col
  dplyr::mutate(
    .data,
    "{{.nest_data}}" := purrr::map({{ .nest_data }}, ~dplyr::slice(.x, !!!dots, .preserve = .preserve))
  )
  
}

#' @export
nest_slice_head <- function(.data,
                            .nest_data,
                            ...,
                            n,
                            prop) {
  
  # tidyeval of dots
  dots <- dplyr::enquos(...)
  
  # map slice_head over list-col
  dplyr::mutate(
    .data,
    "{{.nest_data}}" := purrr::map({{ .nest_data }}, ~dplyr::slice_head(.x, !!!dots, n = n, prop = prop))
  )
  
}

#' @export
nest_slice_tail <- function(.data,
                            .nest_data,
                            ...,
                            n,
                            prop) {
  
  # tidyeval of dots
  dots <- dplyr::enquos(...)
  
  # map slice_tail over list-col
  dplyr::mutate(
    .data,
    "{{.nest_data}}" := purrr::map({{ .nest_data }}, ~dplyr::slice_tail(.x, !!!dots, n = n, prop = prop))
  )
  
}

#' @export
nest_slice_min <- function(.data,
                           .nest_data,
                           order_by, 
                           ...,
                           n,
                           prop,
                           with_ties = TRUE) {
  
  # tidyeval of dots 
  dots <- dplyr::enquos(...)
  
  # map slice_min over list-col
  dplyr::mutate(
    .data,
    "{{.nest_data}}" := purrr::map({{ .nest_data }}, ~dplyr::slice_min(.x, order_by = order_by, !!!dots, n = n, prop = prop, with_ties = with_ties))
  )
  
}

#' @export
nest_slice_max <- function(.data,
                           .nest_data,
                           order_by,
                           ...,
                           n,
                           prop,
                           with_ties = TRUE) {
  
  # tidyeval of dots
  dots <- dplyr::enquos(...) 
  
  # map slice_max over list-col
  dplyr::mutate(
    .data,
    "{{.nest_data}}" := purrr::map({{ .nest_data }}, ~dplyr::slice_max(.x, order_by = order_by, !!!dots, n = n, prop = prop, with_ties = with_ties))
  )
  
}

#' @export
nest_slice_sample <- function(.data,
                              .nest_data,
                              ...,
                              n,
                              prop,
                              weight_by = NULL,
                              replace = FALSE) {
  
  # tidyeval of dots
  dots <- dplyr::enquos(...)
  
  # map slice_sample over list-col
  dplyr::mutate(
    .data,
    "{{.nest_data}}" := purrr::map({{ .nest_data }}, ~dplyr::slice_sample(.x, !!!dots, n = n, prop = prop, weight_by = weight_by, replace = replace))
  )
  
}