# setup for tests
source("nplyr-reusable.R")

test_that("nest_drop_na() returns tibble in expected format", {
  
  gm_nest_na <- gm_nest %>%
    nest_mutate(.nest_data = country_data,year = if_else(1:n() <= 3,year,NA_integer_))
  
  test_format(
    nest_drop_na, 
    .data = gm_nest_na,
    .nest_data = country_data
  )
  
})

test_that("nest_drop_na() drops rows with NAs",{
  
  gm_nest_na <- gm_nest 
  gm_nest_na$country_data[[2]]$year[5] <- NA
  gm_nest_na$country_data[[3]]$pop[10] <- NA
  gm_nest_na$country_data[[6]]$lifeExp[15] <- NA
  
  x <- gm_nest_na %>% nest_drop_na(.nest_data = country_data,year,pop,lifeExp)
  
  expect_equal(nrow(x$country_data[[2]]),1301)
  expect_equal(nrow(x$country_data[[3]]),636)
  expect_equal(nrow(x$country_data[[6]]),186)
})