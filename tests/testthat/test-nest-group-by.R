# setup for tests
source("nplyr-reusable.R")

# ------------------------------nest-group-by-----------------------------------

test_that("nest_group_by() returns tibble in expected format", {
  
  test_format(
    nest_group_by, 
    .data = gm_nest,
    .nest_data = country_data,
    country,
    grouped = TRUE
  )
  
}) 

test_that("nest_group_by() returns nested tibble in expected format", {
  
  x <- nest_group_by(gm_nest, country_data, country)
  expect_named(x$country_data[[1]], c("country", "year", "lifeExp", "pop", "gdpPercap"))
  expect_equal(groups(x$country_data[[1]]) == "country", c(TRUE))
  
})

test_that("nest_group_by() overrides existing groups", {
  
  x <- nest_group_by(gm_nest, country_data, country)
  y <- nest_group_by(x, country_data, year)
  expect_named(y$country_data[[1]], c("country", "year", "lifeExp", "pop", "gdpPercap"))
  expect_equal(groups(y$country_data[[1]]) == "year", c(TRUE))
  
})

test_that("nest_group_by() works as expected with .add = TRUE", {
  
  x <- nest_group_by(gm_nest, country_data, country)
  y <- nest_group_by(x, country_data, year, .add = TRUE)
  expect_named(y$country_data[[1]], c("country", "year", "lifeExp", "pop", "gdpPercap"))
  expect_equal(groups(y$country_data[[1]]) == c("country", "year"), c(TRUE, TRUE))
  
})

# ---------------------------------nest-ungroup---------------------------------

test_that("nest_ungroup() returns tibble in expected format", {
  
  x <- nest_group_by(gm_nest, country_data, country)
  
  test_format(
    nest_ungroup, 
    .data = x,
    .nest_data = country_data
  )
  
}) 

test_that("nest_ungroup() returns a nested tibble in expected format", {
  
  x <- nest_group_by(gm_nest, country_data, country)
  y <- nest_ungroup(x, country_data)
  expect_named(x$country_data[[1]], c("country", "year", "lifeExp", "pop", "gdpPercap"))
  
})

