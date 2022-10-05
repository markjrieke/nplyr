# setup for tests
source("nplyr-reusable.R")

test_that("nest_extract() returns tibble in expected format", {
  
  gm <- gm_nest %>% tidyr::unnest(cols = c(country_data))
  gm_ext <- gm %>% mutate(comb = rep(c(NA, "a-b", "a-d", "b-c", "d-e"),length.out = nrow(gm)))
  gm_nest_ext <- gm_ext %>% tidyr::nest(country_data = -continent)
  
  test_format(
    nest_extract, 
    .data = gm_nest_ext,
    .nest_data = country_data,
    col = comb,
    into = c("var1","var2"),
    regex = "([[:alnum:]]+)-([[:alnum:]]+)"
  )
  
})

test_that("nest_extract() creates correct new columns",{
  
  gm <- gm_nest %>% tidyr::unnest(cols = c(country_data))
  gm_ext <- gm %>% mutate(comb = rep(c(NA, "a-b", "a-d", "b-c", "d-e"),length.out = nrow(gm)))
  gm_nest_ext <- gm_ext %>% tidyr::nest(country_data = -continent)
  
  x <- gm_nest_ext %>% nest_extract(.nest_data = country_data,col = comb,into = c("var1","var2"),regex = "([[:alnum:]]+)-([[:alnum:]]+)")
  
  expect_equal(x$country_data[[5]]$var1,
               rep(c("a", "b", "d", NA, "a"),length.out = 139))
  expect_equal(x$country_data[[5]]$var2,
               rep(c("d", "c", "e", NA, "b"),length.out = 139))
})