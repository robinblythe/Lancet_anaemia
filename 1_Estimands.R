gc()
options(scipen = 999, digits = 5)

library(tidyverse)
library(EnvStats)

source("./0_Functions.R")

# Load in data
import <- "C:/Users/blythe/OneDrive - Queensland University of Technology/WHO anaemia/Population data/"
df_mild <- read.csv(paste0(import, "mild_anemia_prev_data.csv"))
df_moderate <- read.csv(paste0(import, "moderate_anemia_prev_data.csv"))
df_severe <- read.csv(paste0(import, "severe_anemia_prev_data.csv"))

df <- do.call(rbind, list(df_mild, df_moderate, df_severe))
remove(df_mild, df_moderate, df_severe, import)

# Anaemia target population: women of reproductive age (15-49)
# Age group IDs: 8 to 14
agegroup <- seq(8, 14, 1)

# Subset by female, reproductive age, 2021 only, Malawi and Armenia
df_anaemic <- subset(df, sex == "Female" &
                       age_group_id %in% agegroup &
                       year_id == 2021 &
                       measure == "prevalence" &
                       metric_name == "Number"&
                       location_name %in% c("Malawi", "Armenia")) |>
  group_by(rei_name, metric_name, location_name) |>
  summarise(EV_prev = round(sum(val[metric_name == "Number"]), digits = 0),
            min_prev = round(sum(lower[metric_name == "Number"]), digits = 0),
            max_prev = round(sum(upper[metric_name == "Number"]), digits = 0)) |>
  ungroup() |>
  select(-"metric_name")

df_pop <- rbind(df_anaemic,
                obtain_wra(data = df, country = c("Armenia", "Malawi"), agegroup = agegroup),
                obtain_tot(data = df, country = c("Armenia", "Malawi")))






#Create distributions of mild/moderate/severe population - Malawi, aggregated by age
trials = 100000
malawi_prev_mild <- with(subset(df_malawi, rei_name == "Mild anemia"), 
                         rtri(n = trials, min = min_prev, max = max_prev, mode = EV_prev))
malawi_prev_moderate <- with(subset(df_malawi, rei_name == "Moderate anemia"), 
                             rtri(n = trials, min = min_prev, max = max_prev, mode = EV_prev))
malawi_prev_severe <- with(subset(df_malawi, rei_name == "Severe anemia"), 
                           rtri(n = trials, min = min_prev, max = max_prev, mode = EV_prev))

#Estimate distributions of total number of women of reproductive age in Malawi


#Estimate pregnant population - using rates or raw numbers?

#Estimate total population of Malawi


#Interventions: costs per case, effectiveness (RR of being 'cured')
#1 - All women: Staple food fortification

#2 - Pregnant women only: Presumptive anti-malarials
