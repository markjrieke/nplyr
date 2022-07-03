# setup for tests
source("nplyr-reusable.R")

# --------------------------------nest-count------------------------------------

test_that("nest_count() returns tibble in expected format", {
  
  test_format(
    nest_count, 
    .data = gm_nest,
    .nest_data = country_data,
    country
  )
  
}) 

test_that("nest_count() returns nested tibbles in the expected format", {
  
  x <- nest_count(gm_nest, country_data, country)
  expect_named(x$country_data[[1]], c("country", "n"))
  expect_equal(x$country_data[[1]]$n[1], 12)
  
})

test_that("nest_count() works as expected with wt param", {
  
  x <- nest_count(gm_nest, country_data, country, wt = pop)
  expect_named(x$country_data[[1]], c("country", "n"))
  expect_equal(x$country_data[[1]]$n[1], 189884585)
  
})

test_that("nest_count() works as expected with multiple cols to count by", {
  
  x <- nest_count(gm_nest, country_data, country, year)
  expect_named(x$country_data[[1]], c("country", "year", "n"))
  expect_equal(x$country_data[[1]]$n[1], 1)
  
})

test_that("nest_count() works as expected with sort = TRUE", {
  
  x <- nest_count(gm_nest, country_data, country, sort = TRUE)
  expect_named(x$country_data[[1]], c("country", "n"))
  expect_equal(x$country_data[[1]]$country[1], "Japan")
  
})

test_that("nest_count() works as expected with new name", {
  
  x <- nest_count(gm_nest, country_data, country, name = "new")
  expect_named(x$country_data[[1]], c("country", "new"))
  expect_equal(x$country_data[[1]]$new[1], 12)
  
})

# -----------------------------nest-add-count-----------------------------------

test_that("nest_add_count() returns tibble in expected format", {
  
  test_format(
    nest_add_count, 
    .data = gm_nest,
    .nest_data = country_data,
    country
  )
  
}) 

test_that("nest_add_count() returns nested tibbles in the expected format", {
  
  x <- nest_add_count(gm_nest, country_data, country)
  expect_named(x$country_data[[1]], c("country", "year", "lifeExp", "pop", "gdpPercap", "n"))
  expect_equal(x$country_data[[1]]$n[1], 12)
  
})

test_that("nest_add_count() works as expected with wt param", {
  
  x <- nest_add_count(gm_nest, country_data, country, wt = pop)
  expect_named(x$country_data[[1]], c("country", "year", "lifeExp", "pop", "gdpPercap", "n"))
  expect_equal(x$country_data[[1]]$n[1], 189884585)
  
})

test_that("nest_add_count() works as expected with multiple cols to count by", {
  
  x <- nest_add_count(gm_nest, country_data, country, year)
  expect_named(x$country_data[[1]], c("country", "year", "lifeExp", "pop", "gdpPercap", "n"))
  expect_equal(x$country_data[[1]]$n[1], 1)
  
})

test_that("nest_add_count() works as expected with sort = TRUE", {
  
  x <- nest_add_count(gm_nest, country_data, country, sort = TRUE)
  expect_named(x$country_data[[1]], c("country", "year", "lifeExp", "pop", "gdpPercap", "n"))
  expect_equal(x$country_data[[1]]$country[1], "Japan")
  
})

test_that("nest_add_count() works as expected with new name", {
  
  x <- nest_add_count(gm_nest, country_data, country, name = "new")
  expect_named(x$country_data[[1]], c("country", "year", "lifeExp", "pop", "gdpPercap", "new"))
  expect_equal(x$country_data[[1]]$new[1], 12)
  
})
