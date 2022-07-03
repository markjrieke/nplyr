# setup for tests
source("nplyr-reusable.R")

test_that("nest_filter() returns tibble in expected format", {
  
  test_format(
    nest_filter, 
    .data = gm_nest,
    .nest_data = country_data,
    year >= 1965
  )
  
}) 

test_that("nest_filter() returns a nested tibble in expected format", {
  
  x <- nest_filter(gm_nest, country_data, year >= 1965)
  expect_named(x$country_data[[1]], c("country", "year", "lifeExp", "pop", "gdpPercap"))
  expect_equal(x$country_data[[1]]$year[1], 1967)
  
})

test_that("nest_filter() works with multiple cols as expected", {
  
  x <- nest_filter(gm_nest, country_data, year >= 1965, stringr::str_detect(country, "q"))
  expect_named(x$country_data[[1]], c("country", "year", "lifeExp", "pop", "gdpPercap"))
  expect_equal(x$country_data[[1]]$year[1], 1967)
  expect_equal(x$country_data[[1]]$country[1], "Iraq")
  
})
