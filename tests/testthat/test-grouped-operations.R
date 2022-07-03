# setup for tests
source("nplyr-reusable.R")
x <- nest_group_by(gm_nest, country_data, country)

test_that("nest_arrange() works as expected with groups", {
  
  test_format(
    nest_arrange,
    .data = x,
    .nest_data = country_data,
    country,
    grouped = TRUE
  )
  
  y <- x %>% nest_arrange(country_data, pop)
  
  expect_named(y$country_data[[1]], c("country", "year", "lifeExp", "pop", "gdpPercap"))
  expect_length(y$country_data[[1]]$country, 578)
  expect_equal(y$country_data[[1]]$pop[1], 120447)
  
})

test_that("nest_count() works as expected with groups", {
  
  test_format(
    nest_count,
    .data = x,
    .nest_data = country_data,
    country,
    grouped = TRUE
  )
  
  y <- x %>% nest_count(country_data, year)
  
  expect_named(y$country_data[[1]], c("country", "year", "n"))
  expect_length(y$country_data[[1]]$country, 578)
  expect_equal(y$country_data[[1]]$n[1], 1)
  
})

test_that("nest_add_count() works as expected with groups", {
  
  test_format(
    nest_add_count,
    .data = x,
    .nest_data = country_data,
    country,
    grouped = TRUE
  )
  
  y <- x %>% nest_add_count(country_data, year)
  
  expect_named(y$country_data[[1]], c("country", "year", "lifeExp", "pop", "gdpPercap", "n"))
  expect_length(y$country_data[[1]]$country, 578)
  expect_equal(y$country_data[[1]]$n[1], 1)
  
})

test_that("nest_distinct() works as expected with groups", {
  
  test_format(
    nest_distinct,
    .data = x,
    .nest_data = country_data,
    year,
    grouped = TRUE
  )
  
  y <- x %>% nest_distinct(country_data, year)
  
  expect_named(y$country_data[[1]], c("country", "year"))
  expect_length(y$country_data[[1]]$country, 578)
  
})

test_that("nest_filter() works as expected with groups", {
  
  test_format(
    nest_distinct,
    .data = x,
    .nest_data = country_data,
    year == min(year),
    grouped = TRUE
  )
  
  y <- x %>% nest_filter(country_data, year == min(year))
  
  expect_named(y$country_data[[1]], c("country", "year", "lifeExp", "pop", "gdpPercap"))
  expect_length(y$country_data[[1]]$country, 43)
  expect_equal(y$country_data[[1]]$year[1], 1952)
  
})

test_that("nest_mutate() works as expected with groups", {
  
  test_format(
    nest_mutate,
    .data = x,
    .nest_data = country_data,
    med_pop = median(pop),
    grouped = TRUE
  )
  
  y <- x %>% nest_mutate(country_data, med_pop = median(pop))
  
  expect_named(y$country_data[[1]], c("country", "year", "lifeExp", "pop", "gdpPercap", "med_pop"))
  expect_length(y$country_data[[1]]$country, 578)
  expect_equal(y$country_data[[1]]$med_pop[13], 7746810)
  
})

test_that("nest_slice() works as expected with groups", {
  
  test_format(
    nest_slice,
    .data = x,
    .nest_data = country_data,
    1, 3, 5,
    grouped = TRUE
  )
  
  y <- x %>% nest_slice(country_data, 1, 3, 5)
  
  expect_named(y$country_data[[1]], c("country", "year", "lifeExp", "pop", "gdpPercap"))
  expect_length(y$country_data[[1]]$country, 125)
  
})

test_that("nest_slice_min() works as expected with groups", {
  
  test_format(
    nest_slice_min,
    .data = x,
    .nest_data = country_data,
    order_by = pop,
    n = 1,
    grouped = TRUE
  )
  
  y <- x %>% nest_slice_min(country_data, order_by = pop, n = 1)
  
  expect_named(y$country_data[[1]], c("country", "year", "lifeExp", "pop", "gdpPercap"))
  expect_length(y$country_data[[1]]$country, 43)
  expect_equal(y$country_data[[1]]$pop[1], 8425333)
  
})

test_that("nest_slice_max() works as expected with groups", {

  test_format(
    nest_slice_max,
    .data = x,
    .nest_data = country_data,
    order_by = pop,
    n = 1,
    grouped = TRUE
  )

  y <- x %>% nest_slice_max(country_data, order_by = pop, n = 1)

  expect_named(y$country_data[[1]], c("country", "year", "lifeExp", "pop", "gdpPercap"))
  expect_length(y$country_data[[1]]$country, 43)
  expect_equal(y$country_data[[1]]$pop[1], 31889923)

})

test_that("nest_slice_head() works as expected with groups", {
  
  test_format(
    nest_slice_head,
    .data = x,
    .nest_data = country_data,
    n = 3,
    grouped = TRUE
  )
  
  y <- x %>% nest_slice_head(country_data, n = 3)
  
  expect_named(y$country_data[[1]], c("country", "year", "lifeExp", "pop", "gdpPercap"))
  expect_length(y$country_data[[1]]$country, 129)
  
})

test_that("nest_slice_tail() works as expected with groups", {
  
  test_format(
    nest_slice_tail,
    .data = x,
    .nest_data = country_data,
    n = 3,
    grouped = TRUE
  )
  
  y <- x %>% nest_slice_tail(country_data, n = 3)
  
  expect_named(y$country_data[[1]], c("country", "year", "lifeExp", "pop", "gdpPercap"))
  expect_length(y$country_data[[1]]$country, 129)
  
})

test_that("nest_summarise() works as expected with groups", {
  
  test_format(
    nest_summarise,
    .data = x,
    .nest_data = country_data,
    med_pop = median(pop),
    min_year = min(year),
    max_year = max(year),
    grouped = TRUE
  )
  
  y <- nest_summarise(x, country_data, med_pop = median(pop), min_year = min(year), max_year = max(year))
  
  expect_named(y$country_data[[1]], c("country", "med_pop", "min_year", "max_year"))
  expect_length(y$country_data[[1]]$country, 43)
  expect_equal(y$country_data[[1]]$med_pop[2], 7746810)
  expect_equal(y$country_data[[1]]$min_year[2], 1992)
  expect_equal(y$country_data[[1]]$max_year[2], 2007)
  
})



