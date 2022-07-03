# setup for tests
source("nplyr-reusable.R")

test_that("nest_arrange() returns tibble in expected format", {
  
  test_format(
    nest_arrange, 
    .data = gm_nest,
    .nest_data = country_data,
    pop
  )
  
}) 

test_that("nest_arrange() works in ascending order as expected (numeric)", {
  
  x <- nest_arrange(gm_nest, country_data, pop)
  expect_named(x$country_data[[1]], c("country", "year", "lifeExp", "pop", "gdpPercap"))
  expect_equal(x$country_data[[1]]$pop[1], 120447)
  
})

test_that("nest_arrange() works in descending order as expected (numeric)", {
  
  x <- nest_arrange(gm_nest, country_data, desc(pop))
  expect_named(x$country_data[[1]], c("country", "year", "lifeExp", "pop", "gdpPercap"))
  expect_equal(x$country_data[[1]]$pop[1], 1318683096)
  
})

test_that("nest_arrange() works in ascending order as expected (character)", {
  
  x <- nest_arrange(gm_nest, country_data, country)
  expect_named(x$country_data[[1]], c("country", "year", "lifeExp", "pop", "gdpPercap"))
  expect_equal(x$country_data[[1]]$country[1], "Afghanistan")
  
})

test_that("nest_arrange() works in descending order as expected (character)", {
  
  x <- nest_arrange(gm_nest, country_data, desc(country))
  expect_named(x$country_data[[1]], c("country", "year", "lifeExp", "pop", "gdpPercap"))
  expect_equal(x$country_data[[1]]$country[1], "Yemen, Rep.")
  
})
