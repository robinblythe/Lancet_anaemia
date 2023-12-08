gc()
options(scipen = 999, digits = 5)

library(tidyverse)
library(countrycode)

import <- "C:/Users/blythe/OneDrive - Queensland University of Technology/WHO anaemia/Population data/"

country <- "Malawi"
pred_year <- 2030

source("./1_Load_and_wrangle.R")
