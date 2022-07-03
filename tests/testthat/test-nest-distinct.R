# setup for tests
source("nplyr-reusable.R")

test_that("nest_distinct() returns tibble in expected format", {
  
  test_format(
    nest_distinct, 
    .data = gm_nest,
    .nest_data = country_data
  )
  
}) 

test_that("nest_distinct() returns nested tibble in expected format", {
  
  x <- nest_distinct(gm_nest, country_data, country)
  expect_named(x$country_data[[1]], c("country"))
  expect_length(x$country_data[[1]]$country, 43)
  
})

test_that("nest_distinct() works with multiple cols to distinct", {
  
  x <- nest_distinct(gm_nest, country_data, country, year)
  expect_named(x$country_data[[1]], c("country", "year"))
  expect_length(x$country_data[[1]]$country, 578)
  
})

test_that("nest_distinct() works as expected with .keep_all = TRUE", {
  
  x <- nest_distinct(gm_nest, country_data, country, .keep_all = TRUE)
  expect_named(x$country_data[[1]], c("country", "year", "lifeExp", "pop", "gdpPercap"))
  expect_length(x$country_data[[1]]$country, 43)
  
})
