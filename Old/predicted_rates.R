# Analysis:
# First, determine what happens based on current trends
# Take values from 2000-2021 for prevalence and fit smoothing spline to get 2030 preds
# If intending to use uncertainty, sample n for each year using EnvStats::rtri(), place values in tibble with list columns. See bottom of script

# Second, determine what happens if we just freeze current rate but applied to 2030 predicted population levels
# Repeat above but instead, fit the spline to the population estimates and multiply by 2021 rate

# Forecasting method is linear extrapolation from a cubic smoothing spline
# Works well for simple data like this: https://robjhyndman.com/papers/splinefcast.pdf

gc()
options(scipen = 999, digits = 5)
# Local filepath. Use own to load GBD data
import <- "C:/Users/blythe/OneDrive - Queensland University of Technology/Lancet anaemia/Population data/"

library(EnvStats)
library(tidyverse)
library(countrycode)

agegroup <- seq(8, 14, 1) # Women of reproductive age group
pred_year <- 2030 # Year for SDG reassessment

# Load GBD data and standardise country names
df_raw <- do.call(rbind, list(
  read.csv(paste0(import, "mild_anemia_prev_data.csv")),
  read.csv(paste0(import, "moderate_anemia_prev_data.csv")),
  read.csv(paste0(import, "severe_anemia_prev_data.csv"))
)) |>
  as_tibble() |>
  mutate(location_name = countryname(location_name))

remove(import)

# List of countries for analysis
countries <- unique(df_raw$location_name)

# Part 1
# Obtain total prevalence of all anaemia types for years 2000 to 2021 then use that to predict 2030 using linear extrapolation from smooth.spline
# Currently omitting uncertainty - can be incorporated later by sampling from triangular dist (EnvStats::rtri())
df1 <- df_raw |>
  filter(
    sex == "Female",
    age_group_id %in% agegroup,
    measure == "prevalence",
    metric_name == "Number"
  ) |>
  group_by(location_name, year_id, rei_name) |>
  summarise(Prevalence = round(sum(val), digits = 0)) |>
  ungroup() |>
  select(location_name, year_id, rei_name, Prevalence)

# Obtain predictions using smoothing spline for each country via for loop
preds <- list()

for (i in 1:length(countries)) {
  preds$mild[[i]] <- tibble(
    location_name = countries[i],
    year_id = pred_year,
    rei_name = "Mild anemia",
    Prevalence = with(
      subset(df1, location_name == countries[i] & rei_name == "Mild anemia"),
      predict(smooth.spline(x = year_id, y = Prevalence), x = pred_year)$y
    )
  )

  preds$moderate[[i]] <- tibble(
    location_name = countries[i],
    year_id = pred_year,
    rei_name = "Moderate anemia",
    Prevalence = with(
      subset(df1, location_name == countries[i] & rei_name == "Moderate anemia"),
      predict(smooth.spline(x = year_id, y = Prevalence), x = pred_year)$y
    )
  )

  preds$severe[[i]] <- tibble(
    location_name = countries[i],
    year_id = pred_year,
    rei_name = "Severe anemia",
    Prevalence = with(
      subset(df1, location_name == countries[i] & rei_name == "Severe anemia"),
      predict(smooth.spline(x = year_id, y = Prevalence), x = pred_year)$y
    )
  )
}

# Combine tibbles
df_preds <- bind_rows(preds)
remove(preds, i)

# Merge predictions with dataset
# Clarify that predictions are based on current trends
df0 <- full_join(df1, df_preds) |>
  arrange(location_name, year_id, rei_name) |>
  mutate(Description = ifelse(year_id < pred_year, "Observed", "Predicted_trend"))

remove(df_preds, df1, agegroup)



# Part 2
# Now obtain population estimates - all age groups, both sexes, all anaemia types
gc()

df2 <- df_raw |>
  filter(
    measure == "prevalence",
    rei_name == "Mild anemia"
  ) |> # Only need mild for population estimates
  group_by(age_group_id, location_name, year_id, sex) |>
  summarise(Pop_by_age_sex = val[metric_name == "Number"] / val[metric_name == "Rate"]) |>
  ungroup()

# Create inner and outer loop for each country and age group
inner <- list()
outer <- list()
agegroup <- unique(df2$age_group_id)

for (i in 1:length(countries)) {
  for (j in 1:length(agegroup)) {
    inner$male[[agegroup[j]]] <- tibble(
      age_group_id = agegroup[j],
      location_name = countries[i],
      year_id = 2030,
      sex = "Male",
      Pop_by_age_sex = with(
        subset(df2, location_name == countries[i] & sex == "Male" & age_group_id == agegroup[j]),
        predict(smooth.spline(x = year_id, y = Pop_by_age_sex), x = 2030)$y
      )
    )

    inner$female[[agegroup[j]]] <- tibble(
      age_group_id = agegroup[j],
      location_name = countries[i],
      year_id = 2030,
      sex = "Female",
      Pop_by_age_sex = with(
        subset(df2, location_name == countries[i] & sex == "Female" & age_group_id == agegroup[j]),
        predict(smooth.spline(x = year_id, y = Pop_by_age_sex), x = 2030)$y
      )
    )
  }

  outer[[i]] <- bind_rows(inner$male, inner$female)
}

df_preds <- bind_rows(outer) |>
  arrange(location_name, year_id, age_group_id, sex)
remove(inner, outer, df2, i, j, agegroup)

# Now multiply each age group by 2021 rate by anaemia type, age and gender
df_rates <- df_raw |>
  filter(
    measure == "prevalence",
    metric_name == "Rate",
    year_id == 2021
  ) |>
  select(age_group_id, location_name, sex, rei_name, val) |>
  pivot_wider(
    names_from = rei_name,
    values_from = val
  )

agegroup <- seq(8, 14, 1) # Women of reproductive age group

df1 <- full_join(df_preds, df_rates) |>
  mutate(
    Population_mild = Pop_by_age_sex * `Mild anemia`,
    Population_moderate = Pop_by_age_sex * `Moderate anemia`,
    Population_severe = Pop_by_age_sex * `Severe anemia`
  ) |>
  select(-c(Pop_by_age_sex, `Mild anemia`, `Moderate anemia`, `Severe anemia`)) |>
  filter(
    age_group_id %in% agegroup,
    sex == "Female"
  ) |>
  group_by(location_name, year_id) |>
  summarise(
    `Mild anemia` = sum(Population_mild),
    `Moderate anemia` = sum(Population_moderate),
    `Severe anemia` = sum(Population_severe)
  ) |>
  pivot_longer(
    cols = c(`Mild anemia`, `Moderate anemia`, `Severe anemia`),
    names_to = "rei_name",
    values_to = "Prevalence"
  ) |>
  mutate(Description = "Predicted_stable")

df <- full_join(df0, df1) |>
  arrange(location_name, year_id, )

write_delim(df, file = "./prevalence2030.csv")
