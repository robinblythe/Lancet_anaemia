#Functions 1, 2: Obtain prevalence estimates for women of reproductive age and total population

obtain_wra <- function(data, country, agegroup){
  subset(data,
         sex == "Female" &
           rei_name == "Mild anemia" &
           measure_name == "Prevalence" &
           age_group_id %in% agegroup &
           year_id == 2021 &
           location_name %in% country) |>
    group_by(age_group_id, location_name) |>
    summarise(EV_prev = val[metric_name == "Number"]/val[metric_name == "Rate"],
              min_prev = val[metric_name == "Number"]/upper[metric_name == "Rate"],
              max_prev = val[metric_name == "Number"]/lower[metric_name == "Rate"]) |>
    ungroup() |>
    group_by(location_name) |>
    summarise(EV_prev = sum(EV_prev),
              min_prev = sum(min_prev),
              max_prev = sum(max_prev),
              rei_name = "Women of reproductive age") |>
    relocate(rei_name)
}

obtain_tot <- function(data, country){
  subset(data,
         rei_name == "Mild anemia" &
           measure_name == "Prevalence" &
           year_id == 2021 &
           location_name %in% country) |>
    group_by(location_name, age_group_id, sex_id) |>
    summarise(EV_prev = val[metric_name == "Number"]/val[metric_name == "Rate"],
              min_prev = val[metric_name == "Number"]/upper[metric_name == "Rate"],
              max_prev = val[metric_name == "Number"]/lower[metric_name == "Rate"]) |>
    ungroup() |>
    group_by(location_name) |>
    summarise(EV_prev = sum(EV_prev),
              min_prev = sum(min_prev),
              max_prev = sum(max_prev),
              rei_name = "Total population") |>
    relocate(rei_name)
}


