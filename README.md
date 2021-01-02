Simulating data and analysing associations between school performance
and psychiatric diagnoses
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
# Show simulated data
df_tot
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
    ## #   subject_6 <dbl>, event_dg1 <dbl>, event_dg2 <dbl>, event_dg3 <dbl>,
    ## #   fas_dg <chr>, time_dg1 <date>, time_dg2 <date>, time_dg3 <date>,
    ## #   dob <date>, fas <dbl>

Descriptive analyses of simulated data

``` r
source("R/mv01_descr_time_event.r")
table_1_data %>% select(-km_fit)
```

    ## # A tibble: 258 x 10
    ## # Groups:   outcome, chracteristic [63]
    ##    outcome chracteristic value     n n_risk pyears events ci_estimate
    ##    <chr>   <chr>         <chr> <int>  <int>  <dbl>  <dbl>       <dbl>
    ##  1 dg1     all           all   60000  56733 6.76e5    796        4.90
    ##  2 dg2     all           all   60000  56407 6.74e5    470        1.05
    ##  3 dg3     all           all   60000  58734 6.89e5   2797        6.51
    ##  4 dg1     covar_1       0     58173  55041 6.56e5    769        5.16
    ##  5 dg1     covar_1       1      1827   1692 2.02e4     27        1.87
    ##  6 dg2     covar_1       0     58173  54714 6.54e5    442        1.02
    ##  7 dg2     covar_1       1      1827   1693 2.02e4     28        2.08
    ##  8 dg3     covar_1       0     58173  56962 6.68e5   2690        6.52
    ##  9 dg3     covar_1       1      1827   1772 2.06e4    107        6.24
    ## 10 dg1     covar_2       0     53145  50249 6.00e5    580        5.63
    ## # … with 248 more rows, and 2 more variables: ci_conf.high <dbl>,
    ## #   ci_conf.low <dbl>

``` r
summary_age
```

    ## # A tibble: 5 x 4
    ##   time_dg1 time_dg2 time_dg3 measure
    ##      <dbl>    <dbl>    <dbl> <chr>  
    ## 1    22.2     22.1     22.2  mean   
    ## 2     3.56     3.70     3.59 sd     
    ## 3    15.5     15.5     15.6  min    
    ## 4    22.3     22.2     22.1  median 
    ## 5    28.6     28.7     28.8  max

## Modelling and visualization

To model the associations, run `R/mv03_fit_models.r`. To visualize the
associations, run `R/mv04_viz.r`. To further edit tables and figres, run
`R/mv05_tabs_xtra_viz.r`.

## Versions

R version: 3.6.3. Version of packages are shown in the table below.

| Package   | Version |
| :-------- | :------ |
| tidyverse | 1.3.0   |
| openxlsx  | 4.1.4   |
| modelr    | 0.1.6   |
| cowplot   | 1.0.0   |
| GGally    | 1.5.0   |
| survival  | 3.1.8   |
| mgcv      | 1.8.31  |
| lubridate | 1.7.4   |
| broom     | 0.5.5   |
| sf        | 0.8.1   |
| knitr     | 1.28    |
| gridExtra | 2.3     |
| simstudy  | 0.1.15  |
