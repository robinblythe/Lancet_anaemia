# Load interventions based on Sumie and Tori's data

# Costs
df_costs <- vroom("./Data/est_costs.csv", show_col_types = FALSE)

df_costs <- df_costs |>
  arrange(Region, IncomeGroup) |>
  group_by(Region, IncomeGroup) |>
  mutate(
    Code = cur_group_id(),
    location_name = countryname(Country)
  ) |>
  dplyr::select(-Country) |>
  relocate(c(location_name, Code), .before = Region) |>
  ungroup() |>
  filter(location_name %in% df_2030$location_name)

df_costs_regional <- df_costs |>
  select(-c(location_name, Code, IncomeGroup)) |>
  group_by(Region) |>
  summarise_all(mean)

# Effectiveness
# Method of moments transformations applied where roughly symmetrical using https://aushsi.shinyapps.io/ShinyPrior/
intervention_list <- list(
  DailyIron_Preg <- rbeta(iter, shape1 = 11.468, shape2 = 19.786),
  DailyIron_WRA <- rbeta(iter, shape1 = 12.198, shape2 = 16.861),
  Deworm <- rnorm(iter, mean = 0.855, sd = 0.130),
  Staple <- rnorm(iter, mean = 0.755, sd = 0.110),
  ITN <- rnorm(iter, mean = 1.075, sd = 0.227),
  IntIron_Preg <- rnorm(iter, mean = 1.320, sd = 0.245) * DailyIron_Preg,
  IntIron_WRA <- rbeta(iter, shape1 = 14.525, shape2 = 6.285),
  Antimalarial <- rbeta(iter, shape1 = 337.799, shape2 = 36.688),
  MMS <- rnorm(iter, mean = 1.035, sd = 0.059) * DailyIron_WRA
)
