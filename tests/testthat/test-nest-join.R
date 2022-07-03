# setup for tests
source("nplyr-reusable.R")
gm_codes <- read.csv("data/gm_codes.csv")

# ------------------------------nest-inner-join---------------------------------

test_that("nest_inner_join() returns tibble in expected format", {
  
  test_format(
    nest_inner_join, 
    .data = gm_nest,
    .nest_data = country_data,
    y = gm_codes,
    by = "country"
  )
  
})

test_that("nest_inner_join() returns nested tibble in expected format", {
  
  x <- nest_inner_join(gm_nest, country_data, gm_codes, "country")
  expect_named(x$country_data[[1]], c("country", "year", "lifeExp", "pop", "gdpPercap", "iso_alpha", "iso_num"))
  expect_length(x$country_data[[1]]$country, 578)
  
})

test_that("nest_inner_join() works as expected with keep = TRUE", {
  
  x <- nest_inner_join(gm_nest, country_data, gm_codes, "country", keep = TRUE)
  expect_named(x$country_data[[1]], c("country.x", "year", "lifeExp", "pop", "gdpPercap", "country.y", "iso_alpha", "iso_num"))
  expect_length(x$country_data[[1]]$country.x, 578)
  
})

test_that("nest_inner_join() works as expected with suffix param", {
  
  x <- nest_inner_join(gm_nest, country_data, gm_codes, "country", keep = TRUE, suffix = c("_old", "_new"))
  expect_named(x$country_data[[1]], c("country_old", "year", "lifeExp", "pop", "gdpPercap", "country_new", "iso_alpha", "iso_num"))
  expect_length(x$country_data[[1]]$country_old, 578)
  
})

# ------------------------------nest-left-join----------------------------------

test_that("nest_left_join() returns tibble in expected format", {
  
  test_format(
    nest_left_join, 
    .data = gm_nest,
    .nest_data = country_data,
    y = gm_codes,
    by = "country"
  )
  
})

test_that("nest_left_join() returns nested tibble in expected format", {
  
  x <- nest_left_join(gm_nest, country_data, gm_codes, "country")
  expect_named(x$country_data[[1]], c("country", "year", "lifeExp", "pop", "gdpPercap", "iso_alpha", "iso_num"))
  expect_length(x$country_data[[1]]$country, 578)
  
})

test_that("nest_left_join() works as expected with keep = TRUE", {
  
  x <- nest_left_join(gm_nest, country_data, gm_codes, "country", keep = TRUE)
  expect_named(x$country_data[[1]], c("country.x", "year", "lifeExp", "pop", "gdpPercap", "country.y", "iso_alpha", "iso_num"))
  expect_length(x$country_data[[1]]$country.x, 578)
  
})

test_that("nest_left_join() works as expected with suffix param", {
  
  x <- nest_left_join(gm_nest, country_data, gm_codes, "country", keep = TRUE, suffix = c("_old", "_new"))
  expect_named(x$country_data[[1]], c("country_old", "year", "lifeExp", "pop", "gdpPercap", "country_new", "iso_alpha", "iso_num"))
  expect_length(x$country_data[[1]]$country_old, 578)
  
})

# ------------------------------nest-right-join---------------------------------

test_that("nest_right_join() returns tibble in expected format", {
  
  test_format(
    nest_right_join, 
    .data = gm_nest,
    .nest_data = country_data,
    y = gm_codes,
    by = "country"
  )
  
})

test_that("nest_right_join() returns nested tibble in expected format", {
  
  x <- nest_right_join(gm_nest, country_data, gm_codes, "country")
  expect_named(x$country_data[[1]], c("country", "year", "lifeExp", "pop", "gdpPercap", "iso_alpha", "iso_num"))
  expect_length(x$country_data[[1]]$country, 722)
  
})

test_that("nest_right_join() works as expected with keep = TRUE", {
  
  x <- nest_right_join(gm_nest, country_data, gm_codes, "country", keep = TRUE)
  expect_named(x$country_data[[1]], c("country.x", "year", "lifeExp", "pop", "gdpPercap", "country.y", "iso_alpha", "iso_num"))
  expect_length(x$country_data[[1]]$country.x, 722)
  
})

test_that("nest_right_join() works as expected with suffix param", {
  
  x <- nest_right_join(gm_nest, country_data, gm_codes, "country", keep = TRUE, suffix = c("_old", "_new"))
  expect_named(x$country_data[[1]], c("country_old", "year", "lifeExp", "pop", "gdpPercap", "country_new", "iso_alpha", "iso_num"))
  expect_length(x$country_data[[1]]$country_old, 722)
  
})

# ------------------------------nest-full-join----------------------------------

test_that("nest_full_join() returns tibble in expected format", {
  
  test_format(
    nest_full_join, 
    .data = gm_nest,
    .nest_data = country_data,
    y = gm_codes,
    by = "country"
  )
  
})

test_that("nest_full_join() returns nested tibble in expected format", {
  
  x <- nest_full_join(gm_nest, country_data, gm_codes, "country")
  expect_named(x$country_data[[1]], c("country", "year", "lifeExp", "pop", "gdpPercap", "iso_alpha", "iso_num"))
  expect_length(x$country_data[[1]]$country, 722)
  
})

test_that("nest_full_join() works as expected with keep = TRUE", {
  
  x <- nest_full_join(gm_nest, country_data, gm_codes, "country", keep = TRUE)
  expect_named(x$country_data[[1]], c("country.x", "year", "lifeExp", "pop", "gdpPercap", "country.y", "iso_alpha", "iso_num"))
  expect_length(x$country_data[[1]]$country.x, 722)
  
})

test_that("nest_full_join() works as expected with suffix param", {
  
  x <- nest_full_join(gm_nest, country_data, gm_codes, "country", keep = TRUE, suffix = c("_old", "_new"))
  expect_named(x$country_data[[1]], c("country_old", "year", "lifeExp", "pop", "gdpPercap", "country_new", "iso_alpha", "iso_num"))
  expect_length(x$country_data[[1]]$country_old, 722)
  
})

# ------------------------------nest-semi-join----------------------------------

test_that("nest_semi_join() returns tibble in expected format", {
  
  test_format(
    nest_semi_join, 
    .data = gm_nest,
    .nest_data = country_data,
    y = gm_codes,
    by = "country"
  )
  
})

test_that("nest_semi_join() returns nested tibble in expected format", {
  
  x <- nest_semi_join(gm_nest, country_data, gm_codes, "country")
  expect_named(x$country_data[[1]], c("country", "year", "lifeExp", "pop", "gdpPercap"))
  expect_length(x$country_data[[1]]$country, 578)
  
})

# ------------------------------nest-anti-join----------------------------------

test_that("nest_anti_join() returns tibble in expected format", {
  
  test_format(
    nest_anti_join, 
    .data = gm_nest,
    .nest_data = country_data,
    y = gm_codes,
    by = "country"
  )
  
})

test_that("nest_anti_join() returns nested tibble in expected format", {
  
  x <- nest_anti_join(gm_nest, country_data, gm_codes, "country")
  expect_named(x$country_data[[1]], c("country", "year", "lifeExp", "pop", "gdpPercap"))
  expect_length(x$country_data[[1]]$country, 0)
  
})

# ------------------------------nest-nest-join----------------------------------

test_that("nest_nest_join() returns tibble in expected format", {
  
  test_format(
    nest_nest_join, 
    .data = gm_nest,
    .nest_data = country_data,
    y = gm_codes,
    by = "country"
  )
  
})

test_that("nest_nest_join() returns nested tibble in expected format", {
  
  x <- nest_nest_join(gm_nest, country_data, gm_codes, "country")
  expect_named(x$country_data[[1]], c("country", "year", "lifeExp", "pop", "gdpPercap", "y"))
  expect_length(x$country_data[[1]]$country, 578)
  expect_type(x$country_data[[1]]$y, c("list"))
  expect_s3_class(x$country_data[[1]]$y[[1]], c("tbl_df", "tbl", "data.frame"))
  expect_named(x$country_data[[1]]$y[[1]], c("iso_alpha", "iso_num"))
  expect_length(x$country_data[[1]]$y[[1]]$iso_alpha, 1)
  
})
