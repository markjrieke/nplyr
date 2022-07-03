# setup for tests
source("nplyr-reusable.R")

# ------------------------------nest-rename-------------------------------------

test_that("nest_rename() returns tibble in expected format", {
  
  test_format(
    nest_rename, 
    .data = gm_nest,
    .nest_data = country_data,
    population = pop
  )
  
})

test_that("nest_rename() returns a nested tibble in expected format", {
  
  x <- nest_rename(gm_nest, country_data, population = pop)
  expect_named(x$country_data[[1]], c("country", "year", "lifeExp", "population", "gdpPercap"))
  
})

test_that("nest_rename() handles multiple column inputs", {
  
  x <- nest_rename(gm_nest, country_data, population = pop, life_expectancy = lifeExp)
  expect_named(x$country_data[[1]], c("country", "year", "life_expectancy", "population", "gdpPercap"))
  
})

# ------------------------------nest-rename-with--------------------------------

test_that("nest_rename_with() returns tibble in expected format", {
  
  test_format(
    nest_rename_with, 
    .data = gm_nest,
    .nest_data = country_data,
    .fn = stringr::str_to_title,
    .cols = everything()
  )
  
})

test_that("nest_rename_with() returns a nested tibble in expected format", {
  
  x <- nest_rename_with(gm_nest, country_data, stringr::str_to_title)
  expect_named(x$country_data[[1]], c("Country", "Year", "Lifeexp", "Pop", "Gdppercap"))
  
})
