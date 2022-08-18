# setup for tests
source("nplyr-reusable.R")

test_that("nest_fill() returns tibble in expected format", {
  
  gm_nest_na <- gm_nest %>%
    nest_mutate(.nest_data = country_data,year = if_else(1:n() <= 3,year,NA_integer_))
  
  test_format(
    nest_fill, 
    .data = gm_nest_na,
    .nest_data = country_data,
    year
  )
  
})

test_that("nest_fill() fills NAs",{
  
  gm_nest_na <- gm_nest %>%
    nest_mutate(.nest_data = country_data,year = if_else(1:n() <= 3,year,NA_integer_))
  
  x <- gm_nest_na %>% nest_fill(.nest_data = country_data,year,.direction = "down")
  
  expect_equal(x$country_data[[1]]$year[4],1962L)
  expect_equal(x$country_data[[5]]$year[4],2002L)
})