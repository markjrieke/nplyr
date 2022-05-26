#' Group a nested object by one or more variables
#' 
#' @description 
#' Most data operations are done on groups defined by variables.
#' `nest_group_by()` takes an existing list-col of tbls and converts it into a 
#' list-col of grouped tbls where operations are performed "by group". 
#' `nest_ungroup()` removes grouping.
#' 
#' @family grouping functions
#' @inheritParams nest_arrange
#' @param ... In `nest_group_by()`, variables or computations to group by.
#'   Computations are always done on the ungrouped data frames.
#'   To perform computations on grouped data, you need to use a separate 
#'   `nest_mutate()` step before the `nest_group_by()`. 
#'   In `nest_ungroup()`, variables to remove from the grouping.
#' @param .add When `FALSE`, the default, `nest_group_by()` will override 
#'   existing groups. To add to the existing groups, use `.add = TRUE`.
#' @param .drop Drop groups formed by factor levels that don't appear in the 
#'   data? Defaults to `TRUE`. 
#'   
#'   Unlike the equivalent parameter in `dplyr::group_by()`, `.drop` in 
#'   `nest_group_by()` does not automatically infer whether or not the data was 
#'   previously grouped with `.drop = FALSE`, meaning you will need to 
#'   explicitly set `.drop = FALSE` for a sequence of groupings. 
#'   
#'   For more information, please see `dplyr::group_by()` and 
#'   `dplyr::group_by_drop_default()`
#'   
#' @return A tibble or dataframe where `.nest_data` is a list-col of grouped 
#'   data frames with class `group_df`, unless the combination of `...` and 
#'   `.add` yields an empty set of grouping columns, in which case `.nest_data` 
#'   will be a list-col of tibbles.
#'   
#' @export
#' @examples 
#' gapminder_grouped <-
#'   gapminder::gapminder %>%
#'   tidyr::nest(country_data = -continent) %>%
#'   nest_group_by(country_data, year)
#'   
#' # grouping doesn't change the structure of .data, apart from listing
#' # the class in .nest_data as grouped_df.
#' gapminder_grouped
#' 
#' # It changes how it acts with other nplyr verbs:
#' gapminder_grouped %>%
#'   nest_summarise(country_data, 
#'                  pop = mean(pop), 
#'                  lifeExp = mean(lifeExp))
#' 
#' gapminder_grouped %>%
#'   nest_filter(country_data, pop == max(pop))
#'   
#' # to remove grouping, use nest_ungroup
#' gapminder_grouped %>%
#'   nest_ungroup(country_data)
#'   
#' # By default, nest_group_by() overrides existing grouping
#' gapminder_grouped %>%
#'   nest_group_by(country_data, country) %>%
#'   mutate(groups = purrr::map(country_data, group_vars))
#' 
#' # Use .add = TRUE to instead append
#' gapminder_grouped %>%
#'   nest_group_by(country_data, country, .add = TRUE) %>%
#'   mutate(groups = purrr::map(country_data, group_vars))
#'   
#' gapminder_nested <-
#'   gapminder::gapminder %>%
#'   tidyr::nest(country_data = -continent)
#'   
#' # You can group by expressions: this is a short-hand 
#' # for a nest_mutate() followed by a nest_group_by()
#' gapminder_nested %>%
#'   nest_group_by(pop = round(pop, -6)) %>%
#'   mutate(n_groups = purrr::map_int(country_data, n_groups))
#' 
#' # The implicit mutate() step is always performed on the 
#' # ungrouped nested data. Here we get 3 groups in each nested tbl:
#' gapminder_nested %>%
#'   nest_group_by(pop_cut = cut(pop, 3)) %>%
#'   mutate(n_groups = purrr::map_int(country_data, n_groups))
#' 
#' # If you want it to be performed by groups,
#' # you have to use an explicit nest_mutate() call. 
#' # Here we get 3 groups per country in each nested tbl.
#' gapminder_nested %>%
#'   nest_group_by(country_data, country) %>%
#'   nest_mutate(country_data, pop_cut = cut(pop, 3)) %>%
#'   nest_group_by(pop_cut) %>%
#'   mutate(n_groups = purrr::map_int(country_data, n_groups))
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

#' @rdname nest_group_by
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

