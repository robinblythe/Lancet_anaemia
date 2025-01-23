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
      summarise( # Roll up rows by country, year and type of anaemia
        Prevalence = ceiling(sum(val)),
        Prev_low = ceiling(sum(lower)),
        Prev_high = ceiling(sum(upper))
      ) |>
      ungroup() |>
      select(location_name, year_id, rei_name, Prevalence, Prev_low, Prev_high)
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
      reframe( # Divide number by rate to get estimate of total population by year
        # Note: population estimates remain the same if using lower/upper estimates for both number/rate
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

rtri_catch <- function(n, min, mode, max) {
  tryCatch(
    rtri(n, min, mode, max),
    error = function(e) {
      runif(n, min, max)
    }
  )
}

# Population calls must be in quotes
# Note that we can introduce population uncertainty here in the prevalence data, which is recycled over n.iter
simulator <- function(prev_data, country, intervention) {
  # Filter datasets by country
  df_costs <- df_costs |> filter(location_name == country)
  df_coverage <- df_coverage |> filter(location_name == country)
  prev_data <- prev_data |> filter(location_name == country)

  # Determine eligible population for intervention (who receives it)
  Pop_eligible <- case_when(
    intervention %in% c("Iron_Preg", "Antimalarial") ~ "Pop_pregnant",
    intervention == "Iron_WRA" ~ "Pop_wra",
    intervention == "Fortification" ~ "Pop_total"
  )

  # Determine targeted population for intervention (who benefits from it)
  Pop_targeted <- case_when(
    intervention == "Iron_Preg" ~ "Pop_pregnant_anaemic",
    intervention %in% c("Iron_WRA", "Fortification") ~ "Pop_anaemic",
    intervention == "Antimalarial" ~ "Pop_pregnant_malaria_anaemic"
  )

  # Get coverage of intervention
  coverage_max <- as.numeric(df_coverage[, grepl(paste0(intervention, "_max"), names(df_coverage))])
  coverage_current <- as.numeric(df_coverage[, grepl(paste0(intervention, "_current"), names(df_coverage))])

  df <- tibble(
    location_name = country,
    Intervention = intervention,
    Cost =
      df_costs[[intervention]] * # Cost of intervention (sampled)
        max(prev_data[[Pop_eligible]]) * # Eligible population
        (coverage_max - coverage_current), # Delta coverage
    Eff =
      (prev_data[[Pop_targeted]][prev_data$rei_name == "Mild anemia"] -
        prev_data[[Pop_targeted]][prev_data$rei_name == "Mild anemia"] *
          ((1 - coverage_max * (1 - effectiveness[[intervention]])) /
            (1 - coverage_current * (1 - effectiveness[[intervention]])))) *
        YLD_mild +

        (prev_data[[Pop_targeted]][prev_data$rei_name == "Moderate anemia"] -
          prev_data[[Pop_targeted]][prev_data$rei_name == "Moderate anemia"] *
            ((1 - coverage_max * (1 - effectiveness[[intervention]])) /
              (1 - coverage_current * (1 - effectiveness[[intervention]])))) *
          YLD_moderate +

        (prev_data[[Pop_targeted]][prev_data$rei_name == "Severe anemia"] -
          prev_data[[Pop_targeted]][prev_data$rei_name == "Severe anemia"] *
            ((1 - coverage_max * (1 - effectiveness[[intervention]])) /
              (1 - coverage_current * (1 - effectiveness[[intervention]])))) *
          YLD_severe,
    Cost_per_YLD = Cost / Eff
  )
}


# Intervention applicator function
apply_intervention <- function(base_data, cea_table) {
  stage <- list()

  for (i in 1:length(countrylist)) { # For each country
    if (countrylist[i] %in% cea_table$Country) { # Assuming the country has cost-effective interventions to apply
      # Apply the intervention using the formula in the rmarkdown file
      int <- subset(cea_table, Country == countrylist[i])
      coverage_max <- as.numeric(df_coverage[df_coverage$location_name == countrylist[i], grepl(paste0(int$Intervention, "_max"), names(df_coverage))])
      coverage_current <- as.numeric(df_coverage[df_coverage$location_name == countrylist[i], grepl(paste0(int$Intervention, "_current"), names(df_coverage))])

      df_post <- base_data |>
        filter(location_name == countrylist[i]) |>
        mutate(
          Pop_pregnant_malaria_anaemic_n =
            Pop_pregnant_malaria_anaemic *
              ((1 - coverage_max * (1 - mean(effectiveness[[int$Intervention]]))) /
                (1 - coverage_current * (1 - mean(effectiveness[[int$Intervention]])))),
          Pop_pregnant_anaemic_n = case_when(
            int$Intervention == "Antimalarial" ~ Pop_pregnant_anaemic - (Pop_pregnant_malaria_anaemic - Pop_pregnant_malaria_anaemic_n),
            .default =
              Pop_pregnant_anaemic *
                ((1 - coverage_max * (1 - mean(effectiveness[[int$Intervention]]))) /
                  (1 - coverage_current * (1 - mean(effectiveness[[int$Intervention]]))))
          ),
          Pop_anaemic_n = case_when(
            int$Intervention == "Antimalarial" ~ Pop_anaemic - (Pop_pregnant_malaria_anaemic - Pop_pregnant_malaria_anaemic_n),
            int$Intervention == "Iron_Preg" ~ Pop_anaemic - (Pop_pregnant_anaemic - Pop_pregnant_anaemic_n),
            .default =
              Pop_anaemic *
                ((1 - coverage_max * (1 - mean(effectiveness[[int$Intervention]]))) /
                  (1 - coverage_current * (1 - mean(effectiveness[[int$Intervention]]))))
          ),
          YLD_n =
            case_when(
              rei_name == "Mild anemia" ~ Pop_anaemic_n * YLD_mild,
              rei_name == "Moderate anemia" ~ Pop_anaemic_n * YLD_moderate,
              rei_name == "Severe anemia" ~ Pop_anaemic_n * YLD_severe
            )
        )
    } else {
      df_post <- base_data |>
        filter(location_name == countrylist[i]) |>
        mutate(
          Pop_pregnant_anaemic_n = Pop_pregnant_anaemic,
          Pop_pregnant_malaria_anaemic_n = Pop_pregnant_malaria_anaemic,
          Pop_anaemic_n = Pop_anaemic,
          YLD_n =
            case_when(
              rei_name == "Mild anemia" ~ Pop_anaemic_n * YLD_mild,
              rei_name == "Moderate anemia" ~ Pop_anaemic_n * YLD_moderate,
              rei_name == "Severe anemia" ~ Pop_anaemic_n * YLD_severe
            )
        )
    }
    stage[[i]] <- df_post
  }

  df_stage <- do.call(rbind, stage) |>
    select(-c(Pop_pregnant_anaemic, Pop_pregnant_malaria_anaemic, Pop_anaemic, YLD)) |>
    rename(
      Pop_pregnant_anaemic = Pop_pregnant_anaemic_n,
      Pop_pregnant_malaria_anaemic = Pop_pregnant_malaria_anaemic_n,
      Pop_anaemic = Pop_anaemic_n,
      YLD = YLD_n
    )
}


# Replace empty simulation results
replace_empty <- function(data) {
  # Determine how many sims to run for replacement
  n_replace <- length(unique(subset(data, is.na(Pct_change_anaemic))$Iteration))
  # Replace empty rows with re-run data
  replacements <- list()
  for (j in 1:n_replace) {
    # Rerun model
    try(source("1_Run_model.R"))
    df_final <- df_final |>
      rename(
        Pop_pregnant_anaemic_post = Pop_pregnant_anaemic,
        Pop_pregnant_malaria_anaemic_post = Pop_pregnant_malaria_anaemic,
        Pop_anaemic_post = Pop_anaemic,
        YLD_post = YLD
      ) |>
      full_join(df_2030,
        by = join_by(location_name, rei_name, Pop_wra, Pop_total, Pop_pregnant)
      ) |>
      mutate(
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
      summarise(
        anaemic_pre = sum(Pop_anaemic_pre),
        anaemic_post = sum(Pop_anaemic_post),
        YLD_pre = sum(YLD_pre),
        YLD_post = sum(YLD_post)
      ) |>
      mutate(
        Pct_change_anaemic = (anaemic_pre - anaemic_post) / anaemic_pre,
        DALYs_averted = YLD_post - YLD_pre,
        Pct_change_YLD = (YLD_pre - YLD_post) / YLD_pre,
        Iteration = NA_real_
      )

    replacements[[j]] <- df_final
  }
  
  # Identify failed iterations and remove them, then join updates to original dataframe
  failed <- unique(data$Iteration[is.na(data$Pct_change_anaemic)])
  df_updated <- data |>
    filter(!(Iteration  %in% failed)) |>
    rbind.data.frame(do.call(rbind, replacements))
  return(df_updated)
    
}
