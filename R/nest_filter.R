#' @importFrom dplyr enquos
#' @importFrom dplyr mutate
#' @importFrom rlang :=
#' @importFrom purrr map
#' @importFrom dplyr filter
#' 
#' @export
nest_filter <- function(.data,
                        .nest_data,
                        ...,
                        .preserve = FALSE) {
  
  # tidyeval of dots 
  dots <- dplyr::enquos(...)
  
  # map filter over list-col
  dplyr::mutate(
    .data,
    "{{.nest_data}}" := purrr::map({{ .nest_data }}, ~dplyr::filter(.x, !!!dots, .preserve = .preserve))
  )
  
}