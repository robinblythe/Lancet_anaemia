gc()
options(scipen = 999, digits = 5)

library(tidyverse)
library(EnvStats)

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

df_malawi <- subset(
  df,
  sex == "Female" &
    age_group_id %in% agegroup &
    year_id == 2021 &
    measure == "prevalence" &
    metric_name == "Number" &
    location_name == "Malawi"
) |>
  group_by(rei_name) |>
  summarise(
    EV_prev = round(sum(val), digits = 0),
    min_prev = round(sum(lower), digits = 0),
    max_prev = round(sum(upper), digits = 0)
  ) |>
  ungroup()

df_armenia <- subset(
  df,
  sex == "Female" &
    age_group_id %in% agegroup &
    year_id == 2021 &
    measure == "prevalence" &
    metric_name == "Number" &
    location_name == "Armenia"
) |>
  group_by(rei_name) |>
  summarise(
    EV_prev = round(sum(val), digits = 0),
    min_prev = round(sum(lower), digits = 0),
    max_prev = round(sum(upper), digits = 0)
  ) |>
  ungroup()



#Triangular distributions of mild/moderate/severe - Malawi
trials = 100000
malawi_prev_mild <- with(subset(df_malawi, rei_name == "Mild anemia"), 
                         rtri(n = trials, min = min_prev, max = max_prev, mode = EV_prev))
malawi_prev_moderate <- with(subset(df_malawi, rei_name == "Moderate anemia"), 
                             rtri(n = trials, min = min_prev, max = max_prev, mode = EV_prev))
malawi_prev_severe <- with(subset(df_malawi, rei_name == "Severe anemia"), 
                           rtri(n = trials, min = min_prev, max = max_prev, mode = EV_prev))


