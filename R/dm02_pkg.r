
# Read previous scripts
# source("R/dm01_utils.R")

# Load packages

# tidyverse for data management and visualization; cowplot for multipanel
# plotting; mgcv for non-linear
# modelling; lubridicate for working with dates


packages <- c(
  "tidyverse",
  "openxlsx",
  "modelr",
  "cowplot",
  "GGally",
  "survival",
  "mgcv",
  "lubridate",
  "broom",
  "sf",
  "knitr",
  "gridExtra",
  "simstudy"
)

suppressMessages(
  ipak(packages)
)

