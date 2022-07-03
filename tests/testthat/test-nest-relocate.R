# setup for tests
source("nplyr-reusable.R")

# ------------------------------nest-relocate-----------------------------------

test_that("nest_relocate() returns tibble in expected format", {
  
  test_format(
    nest_relocate, 
    .data = gm_nest,
    .nest_data = country_data,
    pop
  )
  
})

test_that("nest_relocate() returns a nested tibble in expected format", {
  
  x <- nest_relocate(gm_nest, country_data, pop)
  expect_named(x$country_data[[1]], c("pop", "country", "year", "lifeExp", "gdpPercap"))
  
})

test_that("nest_relocate() works as expected with multiple cols", {
  
  x <- nest_relocate(gm_nest, country_data, year, pop)
  expect_named(x$country_data[[1]], c("year", "pop", "country", "lifeExp", "gdpPercap"))
  
})

test_that("nest_relocate() works as expected with .before/.after", {
  
  x <- nest_relocate(gm_nest, country_data, pop, .before = year)
  y <- nest_relocate(gm_nest, country_data, pop, .after = country)
  
  expect_named(x$country_data[[1]], c("country", "pop", "year", "lifeExp", "gdpPercap"))
  expect_named(x$country_data[[1]], c("country", "pop", "year", "lifeExp", "gdpPercap"))
  
})
