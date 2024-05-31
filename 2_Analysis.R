gc()
options(scipen = 999, digits = 5)

library(tidyverse)
library(vroom)
library(countrycode)
library(EnvStats)

# Load function script and create df_analysis (2030 prediction data)
# source("./99_Functions.R")
# Run the dataset creation file
# source("./0_Create_dataset.R")
# Or if already run, just load it in from the data folder
df_2030 <- readRDS("./Data/est_2030.rds")

# Assuming 10k iterations
iter <- 10000

# Load in costs and interventions data
source("./1_Interventions.R")

# Create a regional version of prevalence data
df_2030_regional <- left_join(
  df_2030,
  df_costs |>
    select(c(location_name, Region))
) |>
  relocate(Region, .before = year_id) |>
  group_by(Region, rei_name) |>
  select(-c(location_name, year_id)) |>
  summarise_all(mean)


# Analysis goes like this:
# Get league table
# Apply intervention 1 to 2030 population
# Return new prevalence rate post-intervention
# Recalculate league table with new prevalence estimates
# Apply intervention 2
# Repeat until all interventions applied, funding runs out, or WTP threshold crossed

# Intervention 1: Daily iron and folic acid supplementation in pregnant women
# Intervention 2: Daily iron supplementation in women of reproductive age
# Intervention 3: Deworming tablets
# Intervention 4: Staple food fortification
# Intervention 5: Insecticide treated bednets for pregnant women
# Intervention 6: Intermittent iron and folic acid supplementation in pregnant women
# Intervention 7: Intermittent iron supplementation in women of reproductive age
# Intervention 8: Antenatal intermittent antimalarials
# Intervention 9: Micronutrient supplementation (MMS)

# YLDs per person:
# Mild anaemia == 0.005
# Moderate anaemia == 0.053
# Severe anaemia == 0.150

# Calculate league table by region
# First get the prevalence for each anaemia type and multiply it by the effectiveness and the cost/patient







# Simulation function:
# Pass rate and costs as vector of length niter
# Pass population as one of c("Pop_wra", "Pop_total", "Pop_pregnant)
