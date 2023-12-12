gc()
options(scipen = 999, digits = 5)

library(tidyverse)
library(countrycode)
import <- "C:/Users/blythe/OneDrive - Queensland University of Technology/WHO anaemia/Population data/"

#Load requisite datasets; only use countries present in all
c1 <- countryname(unique(read.csv(paste0(import, "mild_anemia_prev_data.csv"))$location_name))
c2 <- countryname(unique(read.csv(paste0(import, "moderate_anemia_prev_data.csv"))$location_name))
c3 <- countryname(unique(read.csv(paste0(import, "severe_anemia_prev_data.csv"))$location_name))
c4 <- suppressWarnings(countryname(unique(na.omit(read.csv(paste0(import, "fertility_rates.csv")))$Country.Name)))
c5 <- suppressWarnings(countryname(unique(na.omit(read.csv(paste0(import, "Stillbirth-rate-and-deaths_2023.csv"))[,1:25])$Country.Name)))

#Save country list
country <- Reduce(intersect, list(c1, c2, c3, c4, c5))
remove(c1, c2, c3, c4, c5, import)
save(country, file = "countries.Rdata")

#Inputs
load("countries.Rdata")
pred_year <- 2030

source("./1_Load_and_wrangle.R")

save(df_anaemic, df_pop, df_wra, file = "data.Rdata")
