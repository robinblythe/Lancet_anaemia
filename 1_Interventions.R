# Load interventions based on Sumie and Tori's data
# Qualify interventions by current coverage

# Costs
df_costs <- vroom("./Data/unit_costs.csv", show_col_types = FALSE) |>
  mutate(Country = case_when(
    Country == "Micronesia" ~ "Micronesia (Federated States of)",
    .default = countryname(Country)
  )) |>
  suppressWarnings() |>
  rename(location_name = Country) |>
  filter(location_name %in% df_2030$location_name)


# Effectiveness
# Method of moments transformations applied where roughly symmetrical using https://aushsi.shinyapps.io/ShinyPrior/
set.seed(888)
effectiveness <- list(
  Iron_Preg = rbeta(iter, shape1 = 11.468, shape2 = 19.786), # DAILY IRON
  Iron_WRA = rbeta(iter, shape1 = 12.198, shape2 = 16.861), # DAILY IRON
  Fortification = rnorm(iter, mean = 0.755, sd = 0.110),
  # IntIron_WRA = rbeta(iter, shape1 = 14.525, shape2 = 6.285),
  Antimalarial = rbeta(iter, shape1 = 337.799, shape2 = 36.688)
)
# effectiveness[["IntIron_Preg"]] <- rnorm(iter, mean = 1.320, sd = 0.245) * effectiveness$DailyIron_Preg

# Intervention coverage
df_coverage <- vroom("./Data/all_coverage_data.csv", show_col_types = FALSE) |>
  na.omit() |>
  suppressMessages()
Encoding(df_coverage$`Country/Economy`) <- "UTF-8"
df_coverage$`Country/Economy` <- iconv(df_coverage$`Country/Economy`, "UTF-8", "UTF-8", sub = "")
df_coverage$`Country/Economy`[df_coverage$`Country/Economy` == "Curaao"] <- "Curacao"

df_coverage <- df_coverage |>
  mutate(location_name = countryname(`Country/Economy`)) |>
  group_by(location_name) |>
  mutate(
    Fortification = max(c(`Wheat flour fortification`, `Rice fortification`, `Maize fortification`)),
    Iron_Preg = `Daily iron & folic acid supplementation`,
    Iron_WRA = `Daily iron & folic acid supplementation`
  ) |>
  rename(Antimalarial = `Antenatal antimalarial`) |>
  select(
    location_name, Iron_Preg, Iron_WRA, Fortification, Antimalarial
  ) |>
  ungroup()
