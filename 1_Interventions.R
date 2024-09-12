# Load interventions based on Sumie and Tori's data
# Qualify interventions by current coverage

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

# Quick fix - Staple currently doesn't have high or low estimates
df_costs$Staple_Low <- df_costs$Staple_Base - 0.01
df_costs$Staple_High <- df_costs$Staple_Base + 0.01


# Effectiveness
# Method of moments transformations applied where roughly symmetrical using https://aushsi.shinyapps.io/ShinyPrior/
set.seed(888)
intervention_list <- list(
  DailyIron_Preg = rbeta(iter, shape1 = 11.468, shape2 = 19.786),
  DailyIron_WRA = rbeta(iter, shape1 = 12.198, shape2 = 16.861),
  Staple = rnorm(iter, mean = 0.755, sd = 0.110),
  IntIron_WRA = rbeta(iter, shape1 = 14.525, shape2 = 6.285),
  Antimalarial = rbeta(iter, shape1 = 337.799, shape2 = 36.688)
)
intervention_list[["IntIron_Preg"]] <- rnorm(iter, mean = 1.320, sd = 0.245) * intervention_list$DailyIron_Preg

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
    Staple = max(c(Staple_wheat, Staple_rice, Staple_maize)),
    DailyIron_Preg = DailyIron,
    DailyIron_WRA = DailyIron
  ) |>
  select(
    location_name, DailyIron_Preg, DailyIron_WRA, Staple, Antimalarial
  ) |>
  ungroup()
