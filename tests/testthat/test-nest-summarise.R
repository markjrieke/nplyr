# setup for tests
source("nplyr-reusable.R")

# ------------------------------nest-summarise----------------------------------

test_that("nest_summarise() returns tibble in expected format", {
  
  test_format(
    nest_summarise, 
    .data = gm_nest,
    .nest_data = country_data,
    pop = mean(pop)
  )
  
})

test_that("nest_summarise() returns a nested tibble in expected format", {
  
  x <- nest_summarise(gm_nest, country_data, pop = mean(pop))
  expect_named(x$country_data[[1]], c("pop"))
  expect_length(x$country_data[[1]]$pop, 1)
  
})