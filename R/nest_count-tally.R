#' @importFrom dplyr enquos
#' @importFrom dplyr mutate
#' @importFrom rlang :=
#' @importFrom purrr map
#' @importFrom dplyr count
#' 
#' @export
nest_count <- function(.data,
                       .nest_data,
                       ...,
                       wt = NULL,
                       sort = FALSE,
                       name = NULL) {
  
  # tidyeval of dots 
  dots <- dplyr::enquos(...)
  
  # map count over list-col
  dplyr::mutate(
    .data,
    "{{.nest_data}}" := purrr::map({{ .nest_data }}, ~dplyr::count(.x, !!!dots, wt = {{ wt }}, sort = sort, name = name))
  )
  
}

#' @importFrom dplyr enquos
#' @importFrom dplyr mutate
#' @importFrom rlang :=
#' @importFrom purrr map
#' @importFrom dplyr add_count
#' 
#' @export
nest_add_count <- function(.data,
                           .nest_data,
                           ...,
                           wt = NULL,
                           sort = FALSE,
                           name = NULL) {
  
  # tidyeval of dots
  dots <- dplyr::enquos(...)
  
  # map add_count over list-col
  dplyr::mutate(
    .data,
    "{{.nest_data}}" := purrr::map({{ .nest_data }}, ~dplyr::add_count(.x, !!!dots, wt = {{ wt }}, sort = sort, name = name))
  )
  
}
