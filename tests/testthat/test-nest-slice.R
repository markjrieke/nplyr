# setup for tests
source("nplyr-reusable.R")

# ------------------------------nest-slice--------------------------------------

test_that("nest_slice() returns tibble in expected format", {
  
  test_format(
    nest_slice, 
    .data = gm_nest,
    .nest_data = country_data,
    1, 3, 5
  )
  
})

test_that("nest_slice() returns nested tibble in expected format", {
  
  x <- nest_slice(gm_nest, country_data, 1, 3, 5)
  expect_named(x$country_data[[1]], c("country", "year", "lifeExp", "pop", "gdpPercap"))
  expect_length(x$country_data[[1]]$country, 3)
  expect_equal(x$country_data[[1]]$year, c(1952, 1962, 1972))
  
  y <- nest_slice(gm_nest, country_data, -1, -3, -5)
  expect_named(y$country_data[[1]], c("country", "year", "lifeExp", "pop", "gdpPercap"))
  expect_length(y$country_data[[1]]$country, 575)
  expect_equal(y$country_data[[1]]$year[1:3], c(1957, 1967, 1977))
  
})

# ------------------------------nest-slice-head---------------------------------

test_that("nest_slice_head() returns tibble in expected format", {
  
  test_format(
    nest_slice_head, 
    .data = gm_nest,
    .nest_data = country_data,
    n = 5
  )
  
})

test_that("nest_slice_head() returns nested tibble in expected format", {
  
  x <- nest_slice_head(gm_nest, country_data, n = 10)
  expect_named(x$country_data[[1]], c("country", "year", "lifeExp", "pop", "gdpPercap"))
  expect_length(x$country_data[[1]]$country, 10)
  expect_equal(x$country_data[[1]]$year, seq(1952, 1997, 5))
  expect_equal(x$country_data[[1]]$country[1], "Afghanistan")
  
  y <- nest_slice_head(gm_nest, country_data, prop = 0.01)
  expect_named(y$country_data[[1]], c("country", "year", "lifeExp", "pop", "gdpPercap"))
  expect_length(y$country_data[[1]]$country, 5)
  expect_equal(y$country_data[[1]]$year, seq(1952, 1972, 5))
  expect_equal(y$country_data[[1]]$country[1], "Afghanistan")
  
})

# ------------------------------nest-slice-tail---------------------------------

test_that("nest_slice_tail() returns tibble in expected format", {
  
  test_format(
    nest_slice_tail, 
    .data = gm_nest,
    .nest_data = country_data,
    n = 5
  )
  
})

test_that("nest_slice_tail() returns nested tibble in expected format", {
  
  x <- nest_slice_tail(gm_nest, country_data, n = 10)
  expect_named(x$country_data[[1]], c("country", "year", "lifeExp", "pop", "gdpPercap"))
  expect_length(x$country_data[[1]]$country, 10)
  expect_equal(x$country_data[[1]]$year, seq(1962, 2007, 5))
  expect_equal(x$country_data[[1]]$country[1], "Yemen, Rep.")
  
  y <- nest_slice_tail(gm_nest, country_data, prop = 0.01)
  expect_named(y$country_data[[1]], c("country", "year", "lifeExp", "pop", "gdpPercap"))
  expect_length(y$country_data[[1]]$country, 5)
  expect_equal(y$country_data[[1]]$year, seq(1987, 2007, 5))
  expect_equal(y$country_data[[1]]$country[1], "Yemen, Rep.")
  
})

# ------------------------------nest-slice-min----------------------------------

test_that("nest_slice_min() returns tibble in expected format", {
  
  test_format(
    nest_slice_min, 
    .data = gm_nest,
    .nest_data = country_data,
    order_by = pop,
    n = 5
  )
  
})

test_that("nest_slice_min() returns a tibble in expected format", {
  
  x <- nest_slice_min(gm_nest, country_data, order_by = pop, n = 5)
  expect_named(x$country_data[[1]], c("country", "year", "lifeExp", "pop", "gdpPercap"))
  expect_length(x$country_data[[1]]$country, 5)
  expect_equal(x$country_data[[1]]$country, c("Bahrain", "Maldives", "Qatar", "Bahrain", "Maldives"))
  expect_equal(x$country_data[[1]]$pop, c(120447, 122681, 131794, 138655, 140721))
  
  y <- nest_slice_min(gm_nest, country_data, order_by = pop, prop = 0.01)
  expect_named(y$country_data[[1]], c("country", "year", "lifeExp", "pop", "gdpPercap"))
  expect_length(y$country_data[[1]]$country, 5)
  expect_equal(y$country_data[[1]]$country, c("Bahrain", "Maldives", "Qatar", "Bahrain", "Maldives"))
  expect_equal(y$country_data[[1]]$pop, c(120447, 122681, 131794, 138655, 140721))
  
})

# ------------------------------nest-slice-max----------------------------------

test_that("nest_slice_max() returns tibble in expected format", {
  
  test_format(
    nest_slice_max, 
    .data = gm_nest,
    .nest_data = country_data,
    order_by = pop,
    n = 5
  )
  
})

test_that("nest_slice_max() returns a tibble in expected format", {
  
  x <- nest_slice_max(gm_nest, country_data, order_by = pop, n = 5)
  expect_named(x$country_data[[1]], c("country", "year", "lifeExp", "pop", "gdpPercap"))
  expect_length(x$country_data[[1]]$country, 5)
  expect_equal(x$country_data[[1]]$country, c(rep("China", 4), "India"))
  expect_equal(x$country_data[[1]]$pop, c(1318683096, 1280400000, 1230075000, 1164970000, 1110396331))
  
  y <- nest_slice_max(gm_nest, country_data, order_by = pop, prop = 0.01)
  expect_named(y$country_data[[1]], c("country", "year", "lifeExp", "pop", "gdpPercap"))
  expect_length(y$country_data[[1]]$country, 5)
  expect_equal(y$country_data[[1]]$country, c(rep("China", 4), "India"))
  expect_equal(y$country_data[[1]]$pop, c(1318683096, 1280400000, 1230075000, 1164970000, 1110396331))
  
})

# ------------------------------nest-slice-sample-------------------------------

test_that("nest_slice_sample() returns tibble in expected format", {
  
  test_format(
    nest_slice_sample, 
    .data = gm_nest,
    .nest_data = country_data,
    n = 5
  )
  
})

test_that("nest_slice_sample() returns a tibble in expected format", {
  
  x <- nest_slice_sample(gm_nest, country_data, n = 5)
  expect_named(x$country_data[[1]], c("country", "year", "lifeExp", "pop", "gdpPercap"))
  expect_length(x$country_data[[1]]$country, 5)
  
  y <- nest_slice_sample(gm_nest, country_data, prop = 0.01)
  expect_named(y$country_data[[1]], c("country", "year", "lifeExp", "pop", "gdpPercap"))
  expect_length(y$country_data[[1]]$country, 5)
  
})

test_that("nest_slice_sample() works as expected with replace = TRUE", {
  
  # find a seed that has some repeats in the sample
  set.seed(1234)
  
  x <- nest_slice_sample(gm_nest, country_data, n = 10, replace = TRUE)
  expect_named(x$country_data[[5]], c("country", "year", "lifeExp", "pop", "gdpPercap"))
  expect_length(x$country_data[[5]]$country, 10)
  expect_equal(x$country_data[[5]]$country[1:2], rep("Latvia", 2))
  expect_equal(x$country_data[[5]]$year[1:2], rep(1978, 2))
  
})

