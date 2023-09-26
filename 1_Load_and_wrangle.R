options(scipen = 999, digits = 5)

library(tidyverse)
library(countrycode)

source("./0_Functions.R")

## Load in data
import <- "C:/Users/blythe/OneDrive - Queensland University of Technology/WHO anaemia/Population data/"

#Anaemia rates from SBD data
df <- do.call(rbind, list(
  read.csv(paste0(import, "mild_anemia_prev_data.csv")),
  read.csv(paste0(import, "moderate_anemia_prev_data.csv")),
  read.csv(paste0(import, "severe_anemia_prev_data.csv"))
  )) |>
  mutate(location_name <- countryname(location_name))

#Live births per woman of reproductive age, 2021
fert <- read.csv(paste0(import, "fertility_rates.csv")) |>
  mutate(location_name = countryname(Country.Name)) |>
  select(-c("Country.Name", "Country.Code", "Indicator.Name", "Indicator.Code")) |>
  pivot_longer(!location_name, names_to = "year_id", values_to = "Fertility_Rate") |>
  mutate(year_id = as.integer(gsub("X", "", year_id))) |>
  filter(year_id >= 1990) |>
  suppressWarnings()
  

#Stillbirth rates per 1000 live births, 2021, from Unicef
#Note stillbirth rate is from 28 weeks/6.5 months
still <- read.csv(paste0(import, "Stillbirth-rate-and-deaths_2023.csv")) |>
  mutate(location_name = countryname(Country.Name)) |>
  select("location_name", "Uncertainty.Bounds.", paste0("X",seq(2000, 2021,1),".5")) |>
  pivot_longer(!c(location_name, Uncertainty.Bounds.), names_to = "year_id") |>
  na.omit() |>
  pivot_wider(names_from = "Uncertainty.Bounds.", values_from = "value") |>
  mutate(year_id = as.integer(gsub(c("X", ".5"), "", year_id))) |>
  suppressWarnings()
  
  
#Replace stillbirth per 1000 live births will stillbirth rate per woman
pregnancy <- inner_join(fert, still) |>
  mutate(still_mid = Median/1000 * Fertility_Rate,
         still_low = Lower/1000 * Fertility_Rate,
         still_high = Upper/1000 * Fertility_Rate) |>
  mutate(Pr_preg_low = ((Fertility_Rate * 0.75) + (still_low * 0.55))/34,
         Pr_preg_med = ((Fertility_Rate * 0.75) + (still_mid * 0.65))/34,
         Pr_preg_high = ((Fertility_Rate * 0.75) + (still_high * 0.75))/34) |>
  select("location_name", "year_id", "Pr_preg_low", "Pr_preg_med", "Pr_preg_high")

remove(fert, still)

# Anaemia target population: women of reproductive age (15-49)
# Age group IDs: 8 to 14
agegroup <- seq(8, 14, 1)

#Use custom functions with Armenia, Malawi
#To apply to entire dataset, use country = unique(df$location_name)
df_prev <- rbind(obtain_anaemic(data = df, country = c("Armenia", "Malawi"), agegroup = agegroup),
                 obtain_wra(data = df, country = c("Armenia", "Malawi"), agegroup = agegroup))

#Pregnant women = women of reproductive age * probability pregnant at a given time

df_preg <- inner_join(obtain_wra(data = df, country = c("Armenia", "Malawi"), agegroup = agegroup),
                      pregnancy) |>
  mutate(rei_name = "Pregnant women",
         EV_prev = EV_prev * Pr_preg_med,
         min_prev = min_prev * Pr_preg_low,
         max_prev = max_prev * Pr_preg_high) |>
  select("location_name", "rei_name", "year_id", "EV_prev", "min_prev", "max_prev") |>
  ungroup()

df_final <- rbind(df_prev, df_preg) |>
  arrange(location_name)

df <- df_final
remove(df_preg, df_prev, df_final, pregnancy, agegroup, import, obtain_anaemic, obtain_tot, obtain_wra)
