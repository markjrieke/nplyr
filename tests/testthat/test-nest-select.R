# setup for tests
source("nplyr-reusable.R")

# ------------------------------nest-select-------------------------------------

test_that("nest_select() returns tibble in expected format", {
  
  test_format(
    nest_select, 
    .data = gm_nest,
    .nest_data = country_data,
    pop
  )
  
})

test_that("nest_select() returns a nested tibble in the expected format", {
  
  x <- nest_select(gm_nest, country_data, pop)
  expect_named(x$country_data[[1]], c("pop"))
  
})

test_that("nest_select() works with multiple selections", {
  
  x <- nest_select(gm_nest, country_data, pop, year)
  expect_named(x$country_data[[1]], c("pop", "year"))
  
})