gc()
options(scipen = 999, digits = 5)

library(tidyverse)
library(vroom)
library(countrycode)
library(EnvStats)
library(zoo)

# Load function script and create df_analysis (2030 prediction data)
# source("./99_Functions.R")
# source("./0_Create_dataset.R")
# Or if already run, just load it in from the data folder
df_2030 <- readRDS("./Data/est_2030.rds")

# Assuming 10k iterations
iter <- 10000

# Load in costs and interventions data
source("./1_Interventions.R")

# List of interventions
interventions <- c(
  "DailyIron_Preg", # Daily iron and folic acid supplementation in pregnant women
  "DailyIron_WRA", # Daily iron supplementation in WRA
  "Deworm", # Deworming tablets in WRA
  "Staple", # Staple food supplementation in all individuals
  "ITN", # Insecticide treated bednets in pregnant women
  "IntIron_Preg", # Intermittent iron and folic acid supplementation in pregnant women
  "IntIron_WRA", # Intermittent iron supplementation in WRA
  "Antimalarial", # Antenatal intermittent antimalarials
  "MMS" # Micronutrient supplements in pregnant women
)

check <- costsim(df_costs, "Armenia", interventions)







# Analysis goes like this:
# Get league table
# Apply intervention 1 to 2030 population
# Return new prevalence rate post-intervention
# Recalculate league table with new prevalence estimates
# Apply intervention 2
# Repeat until all interventions applied, funding runs out, or WTP threshold crossed

# Start with single country example: Armenia
# First get the cost of upping each intervention to 90% individually


# Calculate league table by region
# First get the prevalence for each anaemia type and multiply it by the effectiveness and the cost/patient




# Create a regional version of prevalence data
# df_2030_regional <- left_join(
#   df_2030,
#   df_costs |>
#     select(c(location_name, Region))
# ) |>
#   relocate(Region, .before = year_id) |>
#   group_by(Region, rei_name) |>
#   select(-c(location_name, year_id)) |>
#   summarise_all(mean)



# Simulation function:
# Pass rate and costs as vector of length niter
# Pass population as one of c("Pop_wra", "Pop_total", "Pop_pregnant)
