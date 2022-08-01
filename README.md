Simulating data and analysing associations between school performance
and psychiatric diagnoses
================

[![DOI](https://zenodo.org/badge/519967868.svg)](https://zenodo.org/badge/latestdoi/519967868)

## Introduction

This document shows how to simulate data, perform the analyses and
vizualize the results as described in the article ‘School Performance
and Later Diagnoses of Non-Affective Psychoses, Bipolar Disorder, and
Depression’ published in Acta Psychiatrica Scandinavica
(<DOI:10.1111/acps.13481>): <http://doi.org/10.1111/acps.13481>.

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

    ## # A tibble: 60,000 × 24
    ##        pid d_start    Average covar_1 covar_2 covar_3 covar_4 covar_5 covar_6
    ##      <dbl> <date>       <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
    ##  1 1000001 2003-05-31    8          0       0       0       0       1       0
    ##  2 1000002 2003-05-31    9.67       0       0       0       1       0       0
    ##  3 1000003 2003-05-31    5.17       0       1       0       1       0       1
    ##  4 1000004 2003-05-31    8.83       0       0       0       1       0       1
    ##  5 1000005 2003-05-31    7.67       0       0       0       0       0       0
    ##  6 1000006 2003-05-31    6.33       0       1       0       1       1       1
    ##  7 1000007 2003-05-31    5.33       0       0       0       0       0       1
    ##  8 1000008 2003-05-31    6.83       0       0       1       1       1       1
    ##  9 1000009 2003-05-31    8.5        0       0       0       0       0       1
    ## 10 1000010 2003-05-31    8.17       0       0       0       0       1       0
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

    ## # A tibble: 258 × 10
    ## # Groups:   outcome, chracteristic [63]
    ##    outcome chracteristic value     n n_risk  pyears events ci_estimate
    ##    <chr>   <chr>         <chr> <int>  <int>   <dbl>  <dbl>       <dbl>
    ##  1 dg1     all           all   60000  56733 676415.    796        4.90
    ##  2 dg2     all           all   60000  56407 674391.    470        1.05
    ##  3 dg3     all           all   60000  58734 688971.   2797        6.51
    ##  4 dg1     covar_1       0     58173  55041 656254.    769        5.16
    ##  5 dg1     covar_1       1      1827   1692  20161.     27        1.87
    ##  6 dg2     covar_1       0     58173  54714 654208.    442        1.02
    ##  7 dg2     covar_1       1      1827   1693  20183.     28        2.08
    ##  8 dg3     covar_1       0     58173  56962 668351.   2690        6.52
    ##  9 dg3     covar_1       1      1827   1772  20620.    107        6.24
    ## 10 dg1     covar_2       0     53145  50249 599795.    580        5.63
    ## # … with 248 more rows, and 2 more variables: ci_conf.high <dbl>,
    ## #   ci_conf.low <dbl>

``` r
summary_age
```

    ## # A tibble: 5 × 4
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
