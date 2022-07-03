# setup for tests
source("nplyr-reusable.R")

# ------------------------------nest-mutate-------------------------------------

test_that("nest_mutate() returns tibble in expected format", {
  
  test_format(
    nest_mutate, 
    .data = gm_nest,
    .nest_data = country_data,
    over_under = if_else(lifeExp >= 40, "over 40", "under 40")
  )
  
}) 

test_that("nest_mutate() returns nested tibble in expected format", {
  
  x <- nest_mutate(gm_nest, country_data, over_under = if_else(lifeExp >= 40, "over 40", "under 40"))
  expect_named(x$country_data[[1]], c("country", "year", "lifeExp", "pop", "gdpPercap", "over_under"))
  
})

test_that("nest_mutate() works with .keep as expected", {
  
  x <- nest_mutate(gm_nest, country_data, over_under = if_else(lifeExp >= 40, "over 40", "under 40"), .keep = "used")
  y <- nest_mutate(gm_nest, country_data, over_under = if_else(lifeExp >= 40, "over 40", "under 40"), .keep = "unused")
  z <- nest_mutate(gm_nest, country_data, over_under = if_else(lifeExp >= 40, "over 40", "under 40"), .keep = "none")
  
  expect_named(x$country_data[[1]], c("lifeExp", "over_under"))
  expect_named(y$country_data[[1]], c("country", "year", "pop", "gdpPercap", "over_under"))
  expect_named(z$country_data[[1]], c("over_under"))
  
})

test_that("nest_mutate() works with .before/.after as expected", {
  
  x <- nest_mutate(gm_nest, country_data, over_under = if_else(lifeExp >= 40, "over 40", "under 40"), .before = pop)
  y <- nest_mutate(gm_nest, country_data, over_under = if_else(lifeExp >= 40, "over 40", "under 40"), .after = lifeExp)
  
  expect_named(x$country_data[[1]], c("country", "year", "lifeExp", "over_under", "pop", "gdpPercap"))
  expect_named(y$country_data[[1]], c("country", "year", "lifeExp", "over_under", "pop", "gdpPercap"))
  
})

# ------------------------------nest-transmute----------------------------------

test_that("nest_transmute() returns tibble in expected format", {
  
  x <- nest_transmute(gm_nest, country_data, over_under = if_else(lifeExp >= 40, "over 40", "under 40"))
  
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  expect_named(x, c("country_data"))
  expect_type(x$country_data, "list")
  expect_tibble(x$country_data[[1]], grouped = FALSE)
  expect_named(x$country_data[[1]], c("over_under"))
  
}) 
