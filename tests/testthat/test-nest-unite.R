# setup for tests
source("nplyr-reusable.R")

test_that("nest_unite() returns tibble in expected format", {
  
  test_format(
    nest_unite, 
    .data = gm_nest,
    .nest_data = country_data,
    col = comb,
    country,
    year,
    sep = ":"
  )
  
})

test_that("nest_unite() creates correct new column",{
  
  x <- gm_nest %>% nest_unite(.nest_data = country_data,col = comb,country,year,sep = ":")
  ref <- paste(gm_nest$country_data[[5]]$country,
               gm_nest$country_data[[5]]$year,
               sep = ":")
  
  expect_equal(x$country_data[[5]]$comb,ref)
})