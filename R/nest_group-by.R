#' Group nested data frames by one or more variables
#' 
#' @description 
#' `nest_group_by()` takes a set of nested tbls and converts it to a set of 
#' nested grouped tbls where operations are performed "by group". 
#' `nest_ungroup()` removes grouping.
#' 
#' @details 
#' `nest_group_by()` and `nest_ungroup()` are largely wrappers for 
#' [dplyr::group_by()] and [dplyr::ungroup()] and maintain the functionality of 
#' `group_by()` and `ungroup()` within each nested data frame. For more 
#' information on `group_by()` or `ungroup()`, please refer to the documentation
#' in [`dplyr`](https://dplyr.tidyverse.org/).
#' 
#' @inheritParams nest_select
#' @param ... In `nest_group_by()`, variables or computations to group by. 
#'   Computations are always done on the ungrouped data frames. To perform 
#'   computations on the grouped data, you need to use a separate `mutate()` 
#'   step after the `group_by()`. 
#'   In `nest_ungroup()`, variables to remove from the grouping.
#' @param .add When `FALSE` (the default), `nest_group_by()` will override the 
#'   existing groups. To add to the existing groups, use `.add = TRUE`.
#' @param .drop Drop groups formed by factor levels that don't appear in the 
#'   data? The default is `TRUE` except when `.nest_data` has been previously 
#'   grouped with `.drop = FALSE`. See [dplyr::group_by_drop_default()] for 
#'   details.
#' 
#' @importFrom dplyr enquos
#' @importFrom dplyr mutate
#' @importFrom rlang :=
#' @importFrom purrr map
#' @importFrom dplyr group_by
#' 
#' @export
#' @family grouping functions
#' 
#' @examples 
#' gm_nest <- gapminder::gapminder %>% tidyr::nest(country_data = -continent)
#' 
#' # grouping doesn't change .nest_data, just .nest_data class:
#' gm_nest_grouped <-
#'   gm_nest %>%
#'   nest_group_by(country_data, year)
#' 
#' gm_nest_grouped
#' 
#' # It changes how it acts with other nplyr verbs:
#' gm_nest_grouped %>%
#'   nest_summarise(
#'     country_data,
#'     lifeExp = mean(lifeExp),
#'     pop = mean(pop),
#'     gdpPercap = mean(gdpPercap)
#'   )
#' 
#' # ungrouping removes variable groups:
#' gm_nest_grouped %>% nest_ungroup(country_data)
nest_group_by <- function(.data,
                          .nest_data,
                          ...,
                          .add = FALSE,
                          .drop = TRUE) {
  
  # assertions and checks
  check_nest_data(.data, {{ .nest_data }})
  
  # tidyeval of dots
  dots <- dplyr::enquos(...)
  
  # map group_by over list-col
  dplyr::mutate(
    .data,
    "{{.nest_data}}" := purrr::map({{ .nest_data }}, ~dplyr::group_by(.x, !!!dots, .add = .add, .drop = .drop))
  )
  
}

#' @importFrom dplyr enquos
#' @importFrom dplyr mutate
#' @importFrom rlang :=
#' @importFrom purrr map
#' @importFrom dplyr ungroup
#' 
#' @export
#' 
#' @rdname nest_group_by
nest_ungroup <- function(.data,
                         .nest_data,
                         ...) {
  
  # assertions and checks
  check_nest_data(.data, {{ .nest_data }})
  
  # tidyeval of dots
  dots <- dplyr::enquos(...)
  
  # map ungroup over list-col
  dplyr::mutate(
    .data,
    "{{.nest_data}}" := purrr::map({{ .nest_data }}, ~dplyr::ungroup(.x, !!!dots))
  )
  
}

