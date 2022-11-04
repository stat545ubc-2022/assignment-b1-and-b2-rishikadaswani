RishikaDaswani_AssignmentB1
================
Rishika
2022-10-31

# Assignment B-1: Making a Function

``` r
#loading packages 
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(testthat)
```

    ## 
    ## Attaching package: 'testthat'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     matches

``` r
library(tidyverse)
```

    ## ── Attaching packages
    ## ───────────────────────────────────────
    ## tidyverse 1.3.2 ──

    ## ✔ ggplot2 3.3.6     ✔ purrr   0.3.4
    ## ✔ tibble  3.1.8     ✔ stringr 1.4.1
    ## ✔ tidyr   1.2.1     ✔ forcats 0.5.2
    ## ✔ readr   2.1.3     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ readr::edition_get()   masks testthat::edition_get()
    ## ✖ dplyr::filter()        masks stats::filter()
    ## ✖ purrr::is_null()       masks testthat::is_null()
    ## ✖ dplyr::lag()           masks stats::lag()
    ## ✖ readr::local_edition() masks testthat::local_edition()
    ## ✖ tidyr::matches()       masks testthat::matches(), dplyr::matches()

``` r
library(palmerpenguins)
library(datateachr)
library(gapminder)
library(tibble)
```

## Exercise 1 & 2: Make a Function & Document your Function

``` r
#' Title Bundling group_by() and summarise() workflow - Find the mean values for the grouped by variable.  
#' 
#' @param dataset - The dataset you are working with 
#' @param groupby_variable - I named this variable the groupby_variable as it is the name of the column you wish to group your data by. 
#' @param summarise_variable - Name of the column you want the data from, hence summarise_variable. 
#'
#' @return A tibble of Mean values of summarise_variable presented in the manner you grouped your data by

groupby_summarise <- function(dataset, groupby_variable, summarise_variable, na.rm) {
  dataset %>% group_by({{groupby_variable}}) %>% summarise(mean_val = mean({{summarise_variable}}, na.rm = TRUE))
}
```

## Exercise 3: Include Examples

``` r
#Example 1 - Finding mean body mads (g) for each of the species in the penguins dataset 
groupby_summarise(penguins, species, body_mass_g)
```

    ## # A tibble: 3 × 2
    ##   species   mean_val
    ##   <fct>        <dbl>
    ## 1 Adelie       3701.
    ## 2 Chinstrap    3733.
    ## 3 Gentoo       5076.

``` r
#Example 2 - Finding mean diameter for each tree genus in the Vancouver trees dataset 
groupby_summarise(vancouver_trees, genus_name, diameter)
```

    ## # A tibble: 97 × 2
    ##    genus_name  mean_val
    ##    <chr>          <dbl>
    ##  1 ABIES          12.9 
    ##  2 ACER           10.6 
    ##  3 AESCULUS       23.7 
    ##  4 AILANTHUS      15.9 
    ##  5 ALBIZIA         6   
    ##  6 ALNUS          17.5 
    ##  7 AMELANCHIER     3.21
    ##  8 ARALIA          6.81
    ##  9 ARAUCARIA      11.4 
    ## 10 ARBUTUS        18.4 
    ## # … with 87 more rows

``` r
#Example 3 - Finding the mean life expectancy for each country in the gapminder dataset 
groupby_summarise(gapminder, country, lifeExp)
```

    ## # A tibble: 142 × 2
    ##    country     mean_val
    ##    <fct>          <dbl>
    ##  1 Afghanistan     37.5
    ##  2 Albania         68.4
    ##  3 Algeria         59.0
    ##  4 Angola          37.9
    ##  5 Argentina       69.1
    ##  6 Australia       74.7
    ##  7 Austria         73.1
    ##  8 Bahrain         65.6
    ##  9 Bangladesh      49.8
    ## 10 Belgium         73.6
    ## # … with 132 more rows

## Exercise 4 - Test the Function

``` r
#Expecting error when variable name chosen is not in dataset
test_that("Test groupby_summarise function", {
  expect_error(groupby_summarise(penguins, 
                                 fake_x,
                                 fake_y))
})
```

    ## Test passed 😸

``` r
mean_bodymass <- as_tibble(penguins %>%
  group_by(species) %>%
  summarise(mean_val = mean(body_mass_g, na.rm = TRUE)))

test_that("Testing basic functionality", {
expect_equal(groupby_summarise(penguins, species, body_mass_g, na.rm = TRUE), mean_bodymass)
})
```

    ## Test passed 🥳

``` r
#Expect output to be a tibble 
test_that("Test groupby_summarise function",{
  expect_true(is_tibble(groupby_summarise(penguins, 
                                      species,
                                      body_mass_g)))
})
```

    ## Test passed 🌈
