# Functions to apply to GBD data
# Populations argument can take "anaemic", "wra", and "total"
rollup <- function(population) {
  if (population == "anaemic") { # Anaemic women
    do.call(rbind, list(
      vroom("./Data/mild_anemia_prev_data.csv", show_col_types = FALSE),
      vroom("./Data/moderate_anemia_prev_data.csv", show_col_types = FALSE),
      vroom("./Data/severe_anemia_prev_data.csv", show_col_types = FALSE)
    )) |>
      mutate(location_name = countryname(location_name)) |> # Standardise country names
      as_tibble() |>
      filter(
        sex == "Female", # Only include women
        age_group_id %in% agegroup, # Of reproductive age
        measure == "prevalence", # by prevalence
        metric_name == "Number" # and get the total number
      ) |>
      group_by(location_name, year_id, rei_name) |>
      summarise(Prevalence = ceiling(sum(val))) |> # Roll up rows into groups
      ungroup() |>
      select(location_name, year_id, rei_name, Prevalence)
  } else if (population == "wra") { # Women of reproductive age
    do.call(rbind, list(
      vroom("./Data/mild_anemia_prev_data.csv", show_col_types = FALSE),
      vroom("./Data/moderate_anemia_prev_data.csv", show_col_types = FALSE),
      vroom("./Data/severe_anemia_prev_data.csv", show_col_types = FALSE)
    )) |>
      mutate(location_name = countryname(location_name)) |> # Standardise country names
      as_tibble() |>
      filter(
        sex == "Female", # Only include women
        age_group_id %in% agegroup, # Of reproductive age
        measure == "prevalence",
        rei_name == "Mild anemia"
      ) |>
      group_by(location_name, age_group_id, year_id) |>
      reframe( # Divide prevalence by rate to get estimate of total population by year
        Prevalence = val[metric_name == "Number"] / val[metric_name == "Rate"]
      ) |>
      ungroup() |>
      group_by(location_name, year_id) |>
      summarise( # Aggregate estimates across years
        Pop_wra = sum(Prevalence)
      )
  } else if (population == "total") { # Total population
    do.call(rbind, list(
      vroom("./Data/mild_anemia_prev_data.csv", show_col_types = FALSE),
      vroom("./Data/moderate_anemia_prev_data.csv", show_col_types = FALSE),
      vroom("./Data/severe_anemia_prev_data.csv", show_col_types = FALSE)
    )) |>
      mutate(location_name = countryname(location_name)) |> # Standardise country names
      as_tibble() |>
      filter(
        rei_name == "Mild anemia",
        measure_name == "Prevalence"
      ) |>
      group_by(location_name, age_group_id, sex_id, year_id) |>
      summarise( # Divide prevalence by rate by year and sex for whole of population
        Prev = val[metric_name == "Number"] / val[metric_name == "Rate"]
      ) |>
      ungroup() |>
      group_by(location_name, year_id) |>
      summarise( # Aggregate estimates
        Pop_total = sum(Prev)
      )
  }
}

# Population calls must be in quotes
simulator <- function(prev_data, country, intervention, Pop_eligible, Pop_targeted) {
  df <- tibble(
    location_name = country,
    Intervention = intervention,
    Cost =
      rtri(iter,
        min = df_costs[[paste0(intervention, "_Low")]],
        max = df_costs[[paste0(intervention, "_High")]],
        mode = df_costs[[paste0(intervention, "_Base")]]
      ) *
        df_2030[[Pop_eligible]] *
        (coverage_max - df_coverage[[intervention]]),
    Eff =
      prev_data[[Pop_targeted]][prev_data$rei_name == "Mild anemia"] *
        (1 - coverage_max * (1 - intervention_list[[intervention]])) /
        (1 - df_coverage[[intervention]] * (1 - intervention_list[[intervention]])) *
        YLD_mild +

        prev_data[[Pop_targeted]][prev_data$rei_name == "Moderate anemia"] *
          (1 - coverage_max * (1 - intervention_list[[intervention]])) /
          (1 - df_coverage[[intervention]] * (1 - intervention_list[[intervention]])) *
          YLD_moderate +

        prev_data[[Pop_targeted]][prev_data$rei_name == "Severe anemia"] *
          (1 - coverage_max * (1 - intervention_list[[intervention]])) /
          (1 - df_coverage[[intervention]] * (1 - intervention_list[[intervention]])) *
          YLD_severe
  ) |>
    mutate(
      Cost_per_YLD = Cost / Eff
    )
}
