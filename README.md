Simulating data and analysing associations between school performance
and psychiatric disorders
================

## Introduction

This document shows how to simulate data, perform the analyses and
vizualize the results as described in the manuscript ‘School Performance
and Later Diagnoses of Non-Affective Psychoses, Bipolar Disorder and
Depression’.

## Load functions

Clone the repository from <https://github.com/davgyl/school_sim.git> and
load the functions from the R-scripts.

## Simulate data

``` r
source("R/dm01_utils.R")
source("R/dm02_pkg.r")
source("R/dm03_sim_data.R")
fbc <- df_tot
# Show simulated data
fbc
```

    ## # A tibble: 60,000 x 24
    ##       pid d_start    Average covar_1 covar_2 covar_3 covar_4 covar_5 covar_6
    ##     <dbl> <date>       <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
    ##  1 1.00e6 2003-05-31    8          0       0       0       0       1       0
    ##  2 1.00e6 2003-05-31    9.67       0       0       0       1       0       0
    ##  3 1.00e6 2003-05-31    5.17       0       1       0       1       0       1
    ##  4 1.00e6 2003-05-31    8.83       0       0       0       1       0       1
    ##  5 1.00e6 2003-05-31    7.67       0       0       0       0       0       0
    ##  6 1.00e6 2003-05-31    6.33       0       1       0       1       1       1
    ##  7 1.00e6 2003-05-31    5.33       0       0       0       0       0       1
    ##  8 1.00e6 2003-05-31    6.83       0       0       1       1       1       1
    ##  9 1.00e6 2003-05-31    8.5        0       0       0       0       0       1
    ## 10 1.00e6 2003-05-31    8.17       0       0       0       0       1       0
    ## # … with 59,990 more rows, and 15 more variables: subject_1 <dbl>,
    ## #   subject_2 <dbl>, subject_3 <dbl>, subject_4 <dbl>, subject_5 <dbl>,
    ## #   subject_6 <dbl>, event_dg1 <int>, event_dg2 <int>, event_dg3 <int>,
    ## #   time_dg1 <date>, time_dg2 <date>, time_dg3 <date>, fas_dg <chr>,
    ## #   dob <date>, fas <dbl>

## Descriptive analyses

``` r
source("R/mv01_descr_time_event.r")
table_1_data %>% select(-km_fit)
```

    ## # A tibble: 258 x 10
    ## # Groups:   outcome, chracteristic [63]
    ##    outcome chracteristic value     n n_risk pyears events ci_estimate
    ##    <chr>   <chr>         <chr> <int>  <int>  <dbl>  <int>       <dbl>
    ##  1 dg1     all           all   60000  60000 7.16e5    796       4.45 
    ##  2 dg2     all           all   60000  60000 7.17e5    478       1.00 
    ##  3 dg3     all           all   60000  60000 7.04e5   2898       6.56 
    ##  4 dg1     covar_1       0     58173  58173 6.94e5    769       4.72 
    ##  5 dg1     covar_1       1      1827   1827 2.18e4     27       1.73 
    ##  6 dg2     covar_1       0     58173  58173 6.96e5    450       0.976
    ##  7 dg2     covar_1       1      1827   1827 2.18e4     28       1.93 
    ##  8 dg3     covar_1       0     58173  58173 6.82e5   2785       6.56 
    ##  9 dg3     covar_1       1      1827   1827 2.13e4    113       6.52 
    ## 10 dg1     covar_2       0     53145  53145 6.35e5    580       5.16 
    ## # … with 248 more rows, and 2 more variables: ci_conf.high <dbl>,
    ## #   ci_conf.low <dbl>

``` r
summary_age
```

    ## # A tibble: 5 x 4
    ##   time_dg1 time_dg2 time_dg3 measure
    ##      <dbl>    <dbl>    <dbl> <chr>  
    ## 1    26.8     27.2     23.8  mean   
    ## 2     2.77     2.29     3.99 sd     
    ## 3    15.5     15.5     15.6  min    
    ## 4    27.8     27.9     24.6  median 
    ## 5    28.9     28.9     28.9  max

## Modelling and visualization

To model the associations, run ´R/mv03\_fit\_modelst.r´. To model the
associations, run ´R/mv03\_fit\_modelst.r´. To model the associations,
run ´R/mv03\_fit\_modelst.r´.

## Version of packages

``` r
packageVersion("tidyverse")
```

    ## [1] '1.3.0'

``` r
packageVersion("openxlsx")
```

    ## [1] '4.1.4'

``` r
packageVersion("modelr")
```

    ## [1] '0.1.6'

``` r
packageVersion("cowplot")
```

    ## [1] '1.0.0'

``` r
packageVersion("GGally")
```

    ## [1] '1.5.0'

``` r
packageVersion("survival")
```

    ## [1] '3.1.8'

``` r
packageVersion("mgcv")
```

    ## [1] '1.8.31'

``` r
packageVersion("lubridate")
```

    ## [1] '1.7.4'

``` r
packageVersion("broom")
```

    ## [1] '0.5.5'

``` r
packageVersion("sf")
```

    ## [1] '0.8.1'

``` r
packageVersion("knitr")
```

    ## [1] '1.28'

``` r
packageVersion("gridExtra")
```

    ## [1] '2.3'

``` r
packageVersion("simstudy")
```

    ## [1] '0.1.15'
