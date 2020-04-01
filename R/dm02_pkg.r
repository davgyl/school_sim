# Load packages

# tidyverse for data management and visualization; cowplot for multipanel
# plotting; mgcv for non-linear
# modelling; lubridicate for working with dates

source("R/dm01_utils.R")

packages <- c(
  "tidyverse",
  "openxlsx",
  "modelr",
  "cowplot",
  "survival",
  "mgcv",
  "lubridate",
  "sf",
  "knitr",
  "gridExtra",
  "simstudy"
)

ipak(packages)
