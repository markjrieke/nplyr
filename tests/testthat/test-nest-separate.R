# setup for tests
source("nplyr-reusable.R")

test_that("nest_separate() returns tibble in expected format", {
  
  gm_nest_comb <- gm_nest %>%
    nest_mutate(.nest_data = country_data,comb = paste(country,year,sep = ":"))
  
  test_format(
    nest_separate, 
    .data = gm_nest_comb,
    .nest_data = country_data,
    col = comb,
    into = c("var1","var2"),
    sep = ":"
  )
  
})

test_that("nest_separate() creates correct new columns",{
  
  gm_nest_comb <- gm_nest %>%
    nest_mutate(.nest_data = country_data,comb = paste(country,year,sep = ":"))
  
  x <- gm_nest_comb %>% nest_separate(.nest_data = country_data,col = comb,into = c("var1","var2"),sep = ":",convert = TRUE)
  
  expect_equal(x$country_data[[1]]$country,x$country_data[[1]]$var1)
  expect_equal(x$country_data[[1]]$year,x$country_data[[1]]$var2)
})