# setup for tests
source("nplyr-reusable.R")

test_that("nest_extract() returns tibble in expected format", {
  
  set.seed(123)
  gm_ext <- gm %>% mutate(comb = sample(c(NA, "a-b", "a-d", "b-c", "d-e"),size = nrow(gm),replace = TRUE))
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
  
  set.seed(123)
  gm_ext <- gm %>% mutate(comb = sample(c(NA, "a-b", "a-d", "b-c", "d-e"),size = nrow(gm),replace = TRUE))
  gm_nest_ext <- gm_ext %>% tidyr::nest(country_data = -continent)
  
  x <- gm_nest_ext %>% nest_extract(.nest_data = country_data,col = comb,into = c("var1","var2"),regex = "([[:alnum:]]+)-([[:alnum:]]+)")
  
  expect_equal(x$country_data[[5]]$var1,
               c("d", "d", "a", NA, "b", NA, NA, "a", "b", NA, "a", "d", "a", NA, "d", "a", NA, "a", "a", "a", "d", "d", "b", "a"))
  expect_equal(x$country_data[[5]]$var2,
               c("e", "e", "d", NA, "c", NA, NA, "d", "c", NA, "d", "e", "b", NA, "e", "d", NA, "b", "b", "b", "e", "e", "c", "d"))
})