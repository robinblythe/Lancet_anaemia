#Create dataset to use for analysis

#Define agegroup, year of analysis
agegroup <- seq(8, 14, 1) # Women of reproductive age group
pred_year <- 2030 # Year for SDG reassessment

#Rollup function can take "anaemic" and "total"
#Anaemic population
anaemic <- rollup(population = "anaemic")

#WRA population
wra <- rollup(population = "wra")

#Total population
total <- rollup(population = "total")

gbd <- list(anaemic, wra, total) |>
  reduce(full_join)
remove(anaemic, wra, total)

#Merge with fertility data
#Live births per woman of reproductive age, 2021
fert <- vroom("./Data/fertility_rates.csv", show_col_types = FALSE) |>
  mutate(location_name = countryname(`Country Name`)) |>
  select(-c("Country Name", "Country Code", "Indicator Name", "Indicator Code")) |>
  pivot_longer(!location_name, names_to = "year_id", values_to = "Fertility_Rate") |>
  mutate(year_id = as.integer(gsub("X", "", year_id)),
         Fertility_Rate = na.locf(Fertility_Rate, na.rm = FALSE)) |>
  suppressWarnings() |>
  na.omit()


#Stillbirth rates per 1000 live births, 2021, from Unicef
#Note stillbirth rate is from 28 weeks/6.5 months
still <- vroom("./Data/Stillbirth-rate-and-deaths_2023.csv", show_col_types = FALSE) |>
  mutate(location_name = countryname(Country.Name)) |>
  select("location_name", "Uncertainty.Bounds*", paste0(seq(2000, 2021,1),".5")) |>
  pivot_longer(!c("location_name", "Uncertainty.Bounds*"), names_to = "year_id") |>
  na.omit() |>
  pivot_wider(names_from = "Uncertainty.Bounds*", values_from = "value") |>
  mutate(year_id = floor(as.numeric(year_id))) |>
  select(-c(Lower, Upper)) |>
  suppressWarnings() |>
  suppressMessages()

pregnancy <- inner_join(fert, still) |>
  mutate(still = Median/1000 * Fertility_Rate) |>
  mutate(Pr_pregnant = ((Fertility_Rate * 0.75) + (still * 0.65))/34) |>
  select("location_name", "year_id", "Pr_pregnant")

remove(fert, still)

#Extract countries list for predictions
countries <- intersect(gbd$location_name, pregnancy$location_name)

#Predict 2030 anaemia rates based on existing prevalence using smoothing splines
preds <- list()

for (i in 1:length(countries)) {
  
  preds$mild[[i]] <- tibble(location_name = countries[i],
                            year_id = pred_year,
                            rei_name = "Mild anemia",
                            Prevalence = with(subset(gbd, location_name == countries[i] & rei_name == "Mild anemia"),
                                              predict(smooth.spline(x = year_id, y = Prevalence), x = pred_year)$y))
  
  preds$moderate[[i]] <- tibble(location_name = countries[i],
                                year_id = pred_year,
                                rei_name = "Moderate anemia",
                                Prevalence = with(subset(gbd, location_name == countries[i] & rei_name == "Moderate anemia"),
                                                  predict(smooth.spline(x = year_id, y = Prevalence), x = pred_year)$y))
  
  preds$severe[[i]] <- tibble(location_name = countries[i],
                              year_id = pred_year,
                              rei_name = "Severe anemia",
                              Prevalence = with(subset(gbd, location_name == countries[i] & rei_name == "Severe anemia"),
                                                predict(smooth.spline(x = year_id, y = Prevalence), x = pred_year)$y))
  
  preds$wra[[i]] <- tibble(location_name = countries[i],
                           year_id = pred_year,
                           Pop_wra = with(subset(gbd, location_name == countries[i] & rei_name == "Severe anemia"),
                                          predict(smooth.spline(x = year_id, y = Pop_wra), x = pred_year)$y))
  
  preds$total[[i]] <- tibble(location_name = countries[i],
                             year_id = pred_year,
                             Pop_total = with(subset(gbd, location_name == countries[i] & rei_name == "Severe anemia"),
                                              predict(smooth.spline(x = year_id, y = Pop_total), x = pred_year)$y))
  
  preds$pregnant[[i]] <- tibble(location_name = countries[i],
                                year_id = pred_year,
                                Pr_pregnant = with(subset(pregnancy, location_name == countries[i]),
                                               predict(smooth.spline(x = year_id, y = Pr_pregnant), x = pred_year)$y))
  
}

# Roll up predictions into 2030 dataset
df_analysis <- bind_rows(preds) |>
  group_by(location_name) |>
  arrange(location_name, rei_name) |>
  fill(Pop_wra, .direction = "up") |>
  fill(Pop_total, .direction = "up") |>
  fill(Pr_pregnant, .direction = "up") |>
  na.omit() |>
  mutate(Pop_pregnant = ceiling(Pr_pregnant * Pop_wra),
         Prevalence = ceiling(Prevalence)) |>
  select(-Pr_pregnant)

remove(gbd, preds, i, pregnancy)

saveRDS(df_analysis, file = "./Data/est_2030.rds")

