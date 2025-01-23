# Create datasets to use for analysis
source("99_Functions.R")

library(tidyverse)
library(vroom)
library(countrycode)
library(zoo)

# Define agegroup, year of analysis
agegroup <- seq(8, 14, 1) # Women of reproductive age group
pred_year <- 2030 # Year for SDG reassessment

# Rollup function can take "anaemic" and "total"
# Anaemic population
anaemic <- rollup(population = "anaemic")

# WRA population
wra <- rollup(population = "wra")

# Total population
total <- rollup(population = "total")

gbd <- list(anaemic, wra, total) |>
  reduce(full_join)
remove(anaemic, wra, total)

# Merge with fertility data
# Live births per woman of reproductive age, 2021
fert <- vroom("./Data/fertility_rates.csv", show_col_types = FALSE) |>
  mutate(location_name = countryname(`Country Name`)) |>
  select(-c("Country Name", "Country Code", "Indicator Name", "Indicator Code")) |>
  pivot_longer(!location_name, names_to = "year_id", values_to = "Fertility_Rate") |>
  mutate(
    year_id = as.integer(gsub("X", "", year_id)),
    Fertility_Rate = na.locf(Fertility_Rate, na.rm = FALSE)
  ) |>
  suppressWarnings() |>
  na.omit()


# Stillbirth rates per 1000 live births, 2021, from Unicef
# Note stillbirth rate is from 28 weeks/6.5 months
still <- vroom("./Data/Stillbirth-rate-and-deaths_2023.csv", show_col_types = FALSE) |>
  mutate(location_name = countryname(Country.Name)) |>
  select("location_name", "Uncertainty.Bounds*", "2021.5") |>
  pivot_longer(!c("location_name", "Uncertainty.Bounds*"), names_to = "year_id") |>
  na.omit() |>
  pivot_wider(names_from = "Uncertainty.Bounds*", values_from = "value") |>
  mutate(year_id = floor(as.numeric(year_id))) |>
  suppressWarnings() |>
  suppressMessages()

# Note - we have uncertainty estimates for stillbirth rates, but incorporating them within the simulation is messy. Currently omitted.
pregnancy <- inner_join(fert, still) |>
  mutate(
    still = Median / 1000 * Fertility_Rate,
    still_low = Lower / 1000 * Fertility_Rate,
    still_high = Upper / 1000 * Fertility_Rate,
    Pr_pregnant_mid = ((Fertility_Rate * 0.75) + (still * 0.65)) / 34, # 34 = weeks of pregnancy
    Pr_pregnant_low = ((Fertility_Rate * 0.75) + (still_low * 0.65)) / 34,
    Pr_pregnant_high = ((Fertility_Rate * 0.75) + (still_high * 0.65)) / 34
  ) |>
  select("location_name", "year_id", "Pr_pregnant_mid", "Pr_pregnant_low", "Pr_pregnant_high")

remove(fert, still)

# Combine pregnancy data with GBD
df_analysis <- left_join(gbd, pregnancy) |>
  filter(year_id == 2021)

malaria <- vroom("./Data/malaria_data.csv", show_col_types = FALSE) |>
  filter(
    Metric == "Incidence Rate",
    Year == max(Year)
  ) |>
  mutate(
    Pct_malarial = Value / 1000,
    location_name = countryname(Name)
  ) |>
  select(location_name, Pct_malarial)

df_2030 <- left_join(df_analysis, malaria)
df_2030[is.na(df_2030)] <- 0


# Costs
df_costs <- vroom("./Data/unit_costs.csv",
  show_col_types = TRUE,
  col_types = c(Country = "c", .default = "n")
) |>
  mutate(
    Country = case_when(
      Country == "Micronesia" ~ "Micronesia (Federated States of)",
      .default = countryname(Country)
    )
  ) |>
  na.omit() |>
  suppressWarnings() |>
  rename(location_name = Country) |>
  filter(location_name %in% df_2030$location_name)



# Ensuring costs != 0
df_costs[df_costs == 0] <- 0.01

# Coverage
df_coverage <- purrr::reduce(
  .x = list(
    vroom("./Data/Coverage_data/MaximumCoverage_WRAIron.csv", col_names = c("location_name", "Iron_WRA_max")),
    vroom("./Data/Coverage_data/CurrentCoverage_WRAIron.csv", col_names = c("location_name", "Iron_WRA_current")),
    vroom("./Data/Coverage_data/MaximumCoverage_AntenatalIron.csv", col_names = c("location_name", "Iron_Preg_max")),
    vroom("./Data/Coverage_data/CurrentCoverage_AntenatalIron.csv", col_names = c("location_name", "Iron_Preg_current")),
    vroom("./Data/Coverage_data/MaximumCoverage_IPTp.csv", col_names = c("location_name", "Antimalarial_max")),
    vroom("./Data/Coverage_data/CurrentCoverage_IPTp.csv", col_names = c("location_name", "Antimalarial_current")),
    vroom("./Data/Coverage_data/MaximumCoverage_fortification.csv", col_names = c("location_name", "Fortification_max")),
    vroom("./Data/Coverage_data/CurrentCoverage_fortification.csv", col_names = c("location_name", "Fortification_current"))
  ),
  merge,
  by = "location_name",
  all = TRUE
)

Encoding(df_coverage$location_name) <- "UTF-8"
df_coverage$location_name <- iconv(df_coverage$location_name, "UTF-8", "UTF-8", sub = "")
df_coverage$location_name[df_coverage$location_name == "Curaao"] <- "Curacao"

df_coverage <- df_coverage |>
  mutate(location_name = countryname(location_name)) |>
  na.omit() |>
  suppressMessages()




saveRDS(df_2030, file = "./Data/est_2030.rds")
saveRDS(df_costs, file = "./Data/costs.rds")
saveRDS(df_coverage, file = "./Data/coverage.rds")

rm(list = ls())
gc()
