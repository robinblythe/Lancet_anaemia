# This file is a sensitivity analysis to assess the impact of coverage on the final results.

options(scipen = 999, digits = 5)

library(countrycode)
library(EnvStats)
library(tidyverse)
library(scales)
library(foreach)
library(doParallel)

# Load functions
source("./99_Functions.R")

# Read in 2030 population estimates, costs, and coverage
df_prevalence <- readRDS("./Data/est_2030.rds")
df_costs_base <- readRDS("./Data/costs.rds")
df_coverage_base <- readRDS("./Data/coverage.rds")
df_coverage_base[,c(3, 5, 7, 9)] <- 0

# Add a global WTP threshold
df_costs_base$WTP_Global <- mean(df_costs_base$WTP_Opp_Upper)
df_costs_base$WTP_Near_Infinite <- 10^10

# Using 1 draw per sim
iter <- 1

# Filter by countries with data for all three input data frames
df_prevalence <- df_prevalence |>
  filter(location_name %in% Reduce(intersect, list(
    df_prevalence$location_name,
    df_costs_base$location_name,
    df_coverage_base$location_name
  )))

# Get full country list
countrylist <- unique(df_prevalence$location_name)

# Which WTP threshold to use?
# NB: "Opp_Upper" corresponds to Pichon-Riviere opportunity cost method (base case)
# HCFI_Lower pertains to the 1 x GDP per capita threshold
# Near_Infinite is... nearly infinite. Applies all interventions except malaria treatment in non-malarial regions.
WTP_list <- c("Opp_Upper", "HCFI_Lower", "Near_Infinite")
Threshold <- WTP_list[1]

# Apply WTP threshold
WTP <- df_costs_base |>
  select(
    location_name,
    paste0("WTP_", Threshold)
  ) |>
  rename(
    Country = location_name,
    WTP = paste0("WTP_", Threshold)
  )

# List of interventions:
interventions <- c(
  "Iron_Preg", # Iron and folic acid supplementation in pregnant women
  "Iron_WRA", # Iron supplementation in WRA
  "Fortification", # Staple food supplementation in all individuals
  "Antimalarial" # Antenatal intermittent antimalarials
)

# Set number of iterations
n <- 400

# Prepare parallel computing
cores <- detectCores()
cl <- makeCluster(cores - 2)
registerDoParallel(cl)

# Run model
set.seed(888)
sim_results <- foreach(
  j = 1:n, .combine = "comb", .multicombine = TRUE, .init = list(list(), list()),
  .export = ls(), .packages = c("tidyverse", "EnvStats")
) %dopar% {
  tryCatch(source("1_Run_model.R", local = T))
  
  # Compile results
  list(
    df_final |> # Takes results after all interventions are applied
      rename(
        Pop_pregnant_anaemic_post = Pop_pregnant_anaemic,
        Pop_pregnant_malaria_anaemic_post = Pop_pregnant_malaria_anaemic,
        Pop_anaemic_post = Pop_anaemic,
        YLD_post = YLD
      ) |>
      full_join(df_2030, # Merges with initial data (no interventions applied)
                by = join_by(location_name, rei_name, Pop_wra, Pop_total, Pop_pregnant)
      ) |>
      mutate( # quantifies change between base case and post-intervention results
        Change_anaemic = Pop_anaemic_post - Pop_anaemic,
        Pct_change_anaemic = Change_anaemic / Pop_anaemic
      ) |>
      rename(
        Country = location_name,
        Anaemia_severity = rei_name,
        Pop_pregnant_anaemic_pre = Pop_pregnant_anaemic,
        Pop_pregnant_malaria_anaemic_pre = Pop_pregnant_malaria_anaemic,
        Pop_anaemic_pre = Pop_anaemic,
        YLD_pre = YLD
      ) |>
      group_by(Country) |>
      summarise( # Summarises changes across anaemia types
        anaemic_pre = sum(Pop_anaemic_pre),
        anaemic_post = sum(Pop_anaemic_post),
        YLD_pre = sum(YLD_pre),
        YLD_post = sum(YLD_post)
      ) |>
      mutate(
        Pct_change_anaemic = (anaemic_pre - anaemic_post) / anaemic_pre,
        DALYs_averted = YLD_post - YLD_pre,
        Pct_change_YLD = (YLD_pre - YLD_post) / YLD_pre,
        Iteration = j
      ),
    cea |> # get league table results per simulation
      mutate(Iteration = j)
  )
}

# Compile results
results <- do.call(rbind, sim_results[[1]])
if (Threshold == "Opp_Upper") { # retains league tables if using base case threshold
  league_tables <- do.call(rbind, sim_results[[2]])
  write_csv(league_tables, file = "./Sensitivity_analysis/league_tables.csv")
}

if (nrow(results) > nrow(na.omit(results))) results <- replace_empty(results) # replaces any empty sim results due to errors (shouldn't be any now)

write_csv(results, file = paste0("./Sensitivity_analysis/sim_results_", Threshold, ".csv"))

# Saves targets for easy access later as .csv files
change <- results |>
  group_by(Country) |>
  summarise(
    change_median = median(Pct_change_anaemic, na.rm = T),
    change_low = quantile(Pct_change_anaemic, 0.025, na.rm = T),
    change_high = quantile(Pct_change_anaemic, 0.975, na.rm = T),
    DALYs_prevented_median = median(DALYs_averted, na.rm = T),
    DALYs_prevented_low = quantile(DALYs_averted, 0.975, na.rm = T),
    DALYs_prevented_high = quantile(DALYs_averted, 0.025, na.rm = T)
  )

write_csv(change, file = paste0("./Sensitivity_analysis/targets_", Threshold, ".csv"))

c("Mean" = mean(change$change_median), "Low" = mean(change$change_low), "High" = mean(change$change_high))
