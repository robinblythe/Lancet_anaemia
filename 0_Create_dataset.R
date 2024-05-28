#Create dataset to use for analysis
gc()
options(scipen = 999, digits = 5)

library(tidyverse)
library(vroom)
library(countrycode)
library(EnvStats)

#Define agegroup, year of analysis
agegroup <- seq(8, 14, 1) # Women of reproductive age group
pred_year <- 2030 # Year for SDG reassessment

#Load function script
source("./99_Functions.R")

#Rollup function can take "anaemic" and "total"
#Anaemic population
anaemic <- rollup(population = "anaemic")

#WRA population
wra <- rollup(population = "wra")

#Total population
total <- rollup(population = "total")

gbd <- list(anaemic, wra, total) |>
  reduce(full_join)
remove(anaemic, wra, total)


#Extract countries list for predictions
countries <- unique(gbd$location_name)

#Predict 2030 anaemia rates based on existing prevalence using smoothing splines
preds <- list()

for (i in 1:length(countries)) {
  
  preds$mild[[i]] <- tibble(location_name = countries[i],
                            year_id = pred_year,
                            rei_name = "Mild anemia",
                            Prevalence = with(subset(gbd, location_name == countries[i] & rei_name == "Mild anemia"),
                                              predict(smooth.spline(x = year_id, y = Prevalence), x = pred_year)$y))
  
  preds$moderate[[i]] <- tibble(location_name = countries[i],
                                year_id = pred_year,
                                rei_name = "Moderate anemia",
                                Prevalence = with(subset(gbd, location_name == countries[i] & rei_name == "Moderate anemia"),
                                                  predict(smooth.spline(x = year_id, y = Prevalence), x = pred_year)$y))
  
  preds$severe[[i]] <- tibble(location_name = countries[i],
                              year_id = pred_year,
                              rei_name = "Severe anemia",
                              Prevalence = with(subset(gbd, location_name == countries[i] & rei_name == "Severe anemia"),
                                                predict(smooth.spline(x = year_id, y = Prevalence), x = pred_year)$y))
  
  preds$wra[[i]] <- tibble(location_name = countries[i],
                           year_id = pred_year,
                           Pop_wra = with(subset(gbd, location_name == countries[i] & rei_name == "Severe anemia"),
                                          predict(smooth.spline(x = year_id, y = Pop_wra), x = pred_year)$y))
  
  preds$total[[i]] <- tibble(location_name = countries[i],
                             year_id = pred_year,
                             Pop_total = with(subset(gbd, location_name == countries[i] & rei_name == "Severe anemia"),
                                              predict(smooth.spline(x = year_id, y = Pop_total), x = pred_year)$y))
}

# Roll up predictions and merge them into gbd data
preds <- bind_rows(preds) |>
  group_by(location_name) |>
  fill(Pop_wra, .direction = "downup") |>
  fill(Pop_total, .direction = "downup") |>
  na.omit() 
  
gbd <- full_join(gbd, preds) |>
  arrange(location_name, year_id, rei_name)

remove(preds, i)

# Obtain pregnancy estimates
# Years in GBD data:
years <- unique(gbd$year_id)

#Live births per woman of reproductive age, 2021
fert <- read.csv("./Data/fertility_rates.csv") |>
  mutate(location_name = countryname(Country.Name)) |>
  select(-c("Country.Name", "Country.Code", "Indicator.Name", "Indicator.Code")) |>
  pivot_longer(!location_name, names_to = "year_id", values_to = "Fertility_Rate") |>
  mutate(year_id = as.integer(gsub("X", "", year_id))) |>
  filter(year_id %in% years) |>
  suppressWarnings() |>
  na.omit()


#Stillbirth rates per 1000 live births, 2021, from Unicef
#Note stillbirth rate is from 28 weeks/6.5 months
still <- read.csv("./Data/Stillbirth-rate-and-deaths_2023.csv") |>
  mutate(location_name = countryname(Country.Name)) |>
  select("location_name", "Uncertainty.Bounds.", paste0("X",seq(2000, 2021,1),".5")) |>
  pivot_longer(!c(location_name, Uncertainty.Bounds.), names_to = "year_id") |>
  na.omit() |>
  pivot_wider(names_from = "Uncertainty.Bounds.", values_from = "value") |>
  mutate(year_id = as.integer(gsub(c("X", ".5"), "", year_id))) |>
  suppressWarnings()

