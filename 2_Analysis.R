gc()
options(scipen = 999, digits = 5)

library(tidyverse)
library(vroom)
library(countrycode)
library(EnvStats)
library(zoo)

#Load function script and create df_analysis (2030 prediction data)
source("./99_Functions.R")
#Run the dataset creation file
source("./0_Create_dataset.R")
#Or if already run, just load it in from the data folder
df_2030 <- readRDS("./Data/est_2030.rds")






