# setup for tests
source("nplyr-reusable.R")

test_that("nest_replace_na() returns tibble in expected format", {
  
  gm_nest_na <- gm_nest %>%
    nest_mutate(.nest_data = country_data,year = if_else(1:n() <= 3,year,NA_integer_))
  
  test_format(
    nest_replace_na, 
    .data = gm_nest_na,
    .nest_data = country_data,
    replace = list(year = 1999L)
  )
  
})

test_that("nest_replace_na() replaces NAs",{
  
  gm_nest_na <- gm_nest 
  gm_nest_na$country_data[[2]]$year[5] <- NA
  gm_nest_na$country_data[[3]]$pop[10] <- NA
  gm_nest_na$country_data[[6]]$lifeExp[15] <- NA
  
  x <- gm_nest_na %>% nest_replace_na(.nest_data = country_data,
                                      replace = list(year = 1999,pop = 0,lifeExp = 0))
  
  expect_equal(x$country_data[[2]]$year[5],1999)
  expect_equal(x$country_data[[3]]$pop[10],0)
  expect_equal(x$country_data[[6]]$lifeExp[15],0)
})