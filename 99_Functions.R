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
# Note that we can introduce population uncertainty here in the prevalence data, which is recycled over n.iter
simulator <- function(prev_data, country, intervention) {
  df_costs <- df_costs |> filter(location_name == country)
  df_coverage <- df_coverage |> filter(location_name == country)
  prev_data <- prev_data |> filter(location_name == country)
  Pop_eligible <- case_when(
    intervention %in% c("Iron_Preg", "Antimalarial") ~ "Pop_pregnant",
    intervention == "Iron_WRA" ~ "Pop_wra",
    intervention == "Fortification" ~ "Pop_total"
  )
  Pop_targeted <- case_when(
    intervention == "Iron_Preg" ~ "Pop_pregnant_anaemic",
    intervention %in% c("Iron_WRA", "Fortification") ~ "Pop_anaemic",
    intervention == "Antimalarial" ~ "Pop_pregnant_malaria_anaemic"
  )
  

  df <- tibble(
    location_name = country,
    Intervention = intervention,

    Cost = 
      rtri(iter,
           min = df_costs[[paste0(intervention, "_Low")]],
           max = df_costs[[paste0(intervention, "_High")]],
           mode = df_costs[[paste0(intervention, "_Base")]]
           ) *
      max(prev_data[[Pop_eligible]]) *
        (coverage_max - df_coverage[[intervention]]),
    
    # Effectiveness determined by daily or intermittent
    Eff =
      prev_data[[Pop_targeted]][prev_data$rei_name == "Mild anemia"] *
        (1 - coverage_max * (1 - effectiveness[[intervention]])) /
        (1 - df_coverage[[intervention]] * (1 - effectiveness[[intervention]])) *
        YLD_mild +

        prev_data[[Pop_targeted]][prev_data$rei_name == "Moderate anemia"] *
          (1 - coverage_max * (1 - effectiveness[[intervention]])) /
          (1 - df_coverage[[intervention]] * (1 - effectiveness[[intervention]])) *
          YLD_moderate +

        prev_data[[Pop_targeted]][prev_data$rei_name == "Severe anemia"] *
          (1 - coverage_max * (1 - effectiveness[[intervention]])) /
          (1 - df_coverage[[intervention]] * (1 - effectiveness[[intervention]])) *
          YLD_severe,
    
    Cost_per_YLD = Cost / Eff
    
  ) 
}


# Intervention applicator function
apply_intervention <- function(base_data, cea_table) {
  
  stage <- list()
  
  for (i in 1:length(countrylist)){
    
    if (countrylist[i] %in% cea_table$Country) {
      
      # Apply the intervention using the formula in the rmarkdown file
      int <- subset(cea_table, Country == countrylist[i])

      df_post <- base_data |>
        filter(location_name == countrylist[i]) |>
        mutate(
          Pop_pregnant_malaria_anaemic_n = ceiling(
            Pop_pregnant_malaria_anaemic *
              (1 - coverage_max * (1 - mean(effectiveness[[int$Intervention]]))) /
              (1 - df_coverage[[int$Intervention]][df_coverage$location_name == countrylist[i]] *
                 (1 - mean(effectiveness[[int$Intervention]])))),
          
          Pop_pregnant_anaemic_n = case_when(
            int$Intervention == "Antimalarial" ~ Pop_pregnant_anaemic - (Pop_pregnant_malaria_anaemic - Pop_pregnant_malaria_anaemic_n),
            .default = ceiling(
              Pop_pregnant_anaemic *
                (1 - coverage_max * (1 - mean(effectiveness[[int$Intervention]]))) /
                (1 - df_coverage[[int$Intervention]][df_coverage$location_name == countrylist[i]] *
                   (1 - mean(effectiveness[[int$Intervention]]))))),
            
            Pop_anaemic_n = case_when(
              int$Intervention == "Antimalarial" ~ Pop_anaemic - (Pop_pregnant_malaria_anaemic - Pop_pregnant_malaria_anaemic_n),
              int$Intervention %in% c("DailyIron_Preg", "IntIron_Preg") ~ Pop_anaemic - (Pop_pregnant_anaemic - Pop_pregnant_anaemic_n),
              .default = ceiling(
                Pop_anaemic *
                  (1 - coverage_max * (1 - mean(effectiveness[[int$Intervention]]))) /
                  (1 - df_coverage[[int$Intervention]][df_coverage$location_name == countrylist[i]] *
                     (1 - mean(effectiveness[[int$Intervention]]))))
              ),
          YLD_n =
            case_when(
              rei_name == "Mild anemia" ~ Pop_anaemic_n * 0.005,
              rei_name == "Moderate anemia" ~ Pop_anaemic_n * 0.053,
              rei_name == "Severe anemia" ~ Pop_anaemic_n * 0.150
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
              rei_name == "Mild anemia" ~ Pop_anaemic_n * 0.005,
              rei_name == "Moderate anemia" ~ Pop_anaemic_n * 0.053,
              rei_name == "Severe anemia" ~ Pop_anaemic_n * 0.150
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


