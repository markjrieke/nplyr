
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nplyr <img src="man/figures/logo.png" align="right" width="120" />

**Author:** [Mark Rieke](https://www.thedatadiary.net/about/)<br/>
**License:**
[MIT](https://github.com/markjrieke/nplyr/blob/main/LICENSE)

<!-- badges: start -->

[![R-CMD-check](https://github.com/markjrieke/nplyr/workflows/R-CMD-check/badge.svg)](https://github.com/markjrieke/nplyr/actions)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/nplyr)](https://CRAN.R-project.org/package=nplyr)
[![](https://cranlogs.r-pkg.org/badges/grand-total/nplyr)](https://cran.r-project.org/package=nplyr)
<!-- badges: end -->

## Overview

`{nplyr}` is a grammar of nested data manipulation that allows users to
perform [dplyr](https://dplyr.tidyverse.org/)-like manipulations on data
frames nested within a list-col of another data frame. Most dplyr verbs
have nested equivalents in nplyr. A (non-exhaustive) list of examples:

-   `nest_mutate()` is the nested equivalent of `mutate()`
-   `nest_select()` is the nested equivalent of `select()`
-   `nest_filter()` is the nested equivalent of `filter()`
-   `nest_summarise()` is the nested equivalent of `summarise()`
-   `nest_group_by()` is the nested equivalent of `group_by()`

In the development version, nplyr also supports nested versions of some
[tidyr](https://tidyr.tidyverse.org/) functions:

-   `nest_drop_na()` is the nested equivalent of `drop_na()`
-   `nest_extract()` is the nested equivalent of `extract()`
-   `nest_fill()` is the nested equivalent of `fill()`
-   `nest_replace_na()` is the nested equivalent of `replace_na()`
-   `nest_separate()` is the nested equivalent of `separate()`
-   `nest_unite()` is the nested equivalent of `unite()`

nplyr is largely a wrapper for dplyr. For the most up-to-date
information on dplyr please visit [dplyr’s
website](https://dplyr.tidyverse.org). If you are new to dplyr, the best
place to start is the [data transformation
chapter](https://r4ds.had.co.nz/transform.html) in R for data science.

## Installation

You can install the released version of nplyr from CRAN or the
development version from github with the
[devtools](https://cran.r-project.org/package=devtools) or
[remotes](https://cran.r-project.org/package=remotes) package:

``` r
# install from CRAN
install.packages("nplyr")

# install from github
devtools::install_github("markjrieke/nplyr")
```

## Usage

To get started, we’ll create a nested column for the country data within
each continent from the
[gapminder](https://CRAN.R-project.org/package=gapminder) dataset.

``` r
library(nplyr)

gm_nest <- 
  gapminder::gapminder_unfiltered %>%
  tidyr::nest(country_data = -continent)

gm_nest
#> # A tibble: 6 × 2
#>   continent country_data        
#>   <fct>     <list>              
#> 1 Asia      <tibble [578 × 5]>  
#> 2 Europe    <tibble [1,302 × 5]>
#> 3 Africa    <tibble [637 × 5]>  
#> 4 Americas  <tibble [470 × 5]>  
#> 5 FSU       <tibble [139 × 5]>  
#> 6 Oceania   <tibble [187 × 5]>
```

dplyr can perform operations on the top-level data frame, but with
nplyr, we can perform operations on the nested data frames:

``` r
gm_nest_example <- 
  gm_nest %>%
  nest_filter(country_data, year == max(year)) %>%
  nest_mutate(country_data, pop_millions = pop/1000000)

# each nested tibble is now filtered to the most recent year
gm_nest_example
#> # A tibble: 6 × 2
#>   continent country_data     
#>   <fct>     <list>           
#> 1 Asia      <tibble [43 × 6]>
#> 2 Europe    <tibble [34 × 6]>
#> 3 Africa    <tibble [53 × 6]>
#> 4 Americas  <tibble [33 × 6]>
#> 5 FSU       <tibble [9 × 6]> 
#> 6 Oceania   <tibble [11 × 6]>

# if we unnest, we can see that a new column for pop_millions has been added
gm_nest_example %>%
  slice_head(n = 1) %>%
  tidyr::unnest(country_data)
#> # A tibble: 43 × 7
#>    continent country           year lifeExp        pop gdpPercap pop_millions
#>    <fct>     <fct>            <int>   <dbl>      <int>     <dbl>        <dbl>
#>  1 Asia      Afghanistan       2007    43.8   31889923      975.       31.9  
#>  2 Asia      Azerbaijan        2007    67.5    8017309     7709.        8.02 
#>  3 Asia      Bahrain           2007    75.6     708573    29796.        0.709
#>  4 Asia      Bangladesh        2007    64.1  150448339     1391.      150.   
#>  5 Asia      Bhutan            2007    65.6    2327849     4745.        2.33 
#>  6 Asia      Brunei            2007    77.1     386511    48015.        0.387
#>  7 Asia      Cambodia          2007    59.7   14131858     1714.       14.1  
#>  8 Asia      China             2007    73.0 1318683096     4959.     1319.   
#>  9 Asia      Hong Kong, China  2007    82.2    6980412    39725.        6.98 
#> 10 Asia      India             2007    64.7 1110396331     2452.     1110.   
#> # … with 33 more rows
#> # ℹ Use `print(n = ...)` to see more rows
```

nplyr also supports grouped operations with `nest_group_by()`:

``` r
gm_nest_example <- 
  gm_nest %>%
  nest_group_by(country_data, year) %>%
  nest_summarise(
    country_data, 
    n = n(),
    lifeExp = median(lifeExp),
    pop = median(pop),
    gdpPercap = median(gdpPercap)
  )

gm_nest_example
#> # A tibble: 6 × 2
#>   continent country_data     
#>   <fct>     <list>           
#> 1 Asia      <tibble [58 × 5]>
#> 2 Europe    <tibble [58 × 5]>
#> 3 Africa    <tibble [13 × 5]>
#> 4 Americas  <tibble [57 × 5]>
#> 5 FSU       <tibble [44 × 5]>
#> 6 Oceania   <tibble [56 × 5]>

# unnesting shows summarised tibbles for each continent
gm_nest_example %>%
  slice(2) %>%
  tidyr::unnest(country_data)
#> # A tibble: 58 × 6
#>    continent  year     n lifeExp      pop gdpPercap
#>    <fct>     <int> <int>   <dbl>    <dbl>     <dbl>
#>  1 Europe     1950    22    65.8 7408264      6343.
#>  2 Europe     1951    18    65.7 7165515      6509.
#>  3 Europe     1952    31    65.9 7124673      5210.
#>  4 Europe     1953    17    67.3 7346100      6774.
#>  5 Europe     1954    17    68.0 7423300      7046.
#>  6 Europe     1955    17    68.5 7499400      7817.
#>  7 Europe     1956    17    68.5 7575800      8224.
#>  8 Europe     1957    31    67.5 7363802      6093.
#>  9 Europe     1958    18    69.6 8308052.     8833.
#> 10 Europe     1959    18    69.6 8379664.     9088.
#> # … with 48 more rows
#> # ℹ Use `print(n = ...)` to see more rows
```

More examples can be found in the package vignettes and function
documentation.

## Bug reports/feature requests

If you notice a bug, want to request a new feature, or have
recommendations on improving documentation, please [open an
issue](https://github.com/markjrieke/nplyr/issues) in the package
repository.
