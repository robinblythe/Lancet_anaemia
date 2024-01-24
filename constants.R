library(countrycode)
library(EnvStats)
library(ggplot2)
library(data.table)

# Simulation data
trials <- 100000
from_year <- 2021
pred_year <- 2030
interventionlist <- c("Staple foods supplementation", "Anti-malarial pregnancy chemoprevention")

load(file = "countries.Rdata")
load(file = "data.Rdata")

source("./0_Functions.R")

# Intervention data
staple_cost_per <- rtri(trials, min = 0.10, mode = 0.12, max = 0.18)
staple_eff_per <- rtri(trials, min = 0.975, mode = 0.976, max = 0.978)
staple_coverage <- rtri(trials, min = 0.9, mode = 0.95, max = 0.98)
malarials_cost_per <- rtri(trials, min = 2.70, mode = 3.64, max = 8.20)
malarials_eff_per <- rtri(trials, min = 0.4, mode = 0.6, max = 0.8)
malarials_coverage <- rtri(trials, min = 0.5, mode = 0.6, max = 0.7)
malaria_weights <- c(0, 1)

# As per GBD study: https://doi.org/10.1186/s13045-021-01202-2
# Disability weight * prevalence = YLD
burden_mild <- rtri(trials, min = 0.001, mode = 0.004, max = 0.008)
burden_moderate <- rtri(trials, min = 0.034, mode = 0.052, max = 0.076)
burden_severe <- rtri(trials, min = 0.101, mode = 0.149, max = 0.209)


# Create modelled estimates

run_sim_YLD <- function(country, interventionlist) {
  df <- simulator(
    country = country,
    year = pred_year,
    pop_wra = df_wra,
    pop_anaemic = df_anaemic,
    malaria_weight = 0,
    interventionlist = interventionlist,
    trials = trials
  )

  baseline <- subset(
    df_anaemic,
    Country == country &
      Year %in% c(2021, pred_year)
  ) |>
    select(-c(Pr_pregnant, Pr_pregnant_lower, Pr_pregnant_upper)) |>
    mutate(
      Model = 0,
      EV = case_when(
        Population == "Mild anemia" ~ (EV * burden_mild[2]),
        Population == "Moderate anemia" ~ (EV * burden_moderate[2]),
        Population == "Severe anemia" ~ (EV * burden_severe[2])
      ),
      EV_lower = case_when(
        Population == "Mild anemia" ~ (EV_lower * burden_mild[1]),
        Population == "Moderate anemia" ~ (EV_lower * burden_moderate[1]),
        Population == "Severe anemia" ~ (EV_lower * burden_severe[1])
      ),
      EV_upper = case_when(
        Population == "Mild anemia" ~ (EV_upper * burden_mild[3]),
        Population == "Moderate anemia" ~ (EV_upper * burden_moderate[3]),
        Population == "Severe anemia" ~ (EV_upper * burden_severe[3])
      )
    )


  diff_mild <- data.frame(
    Country = country,
    Population = "Mild anemia",
    Year = pred_year,
    EV = mean(df$mild_post_2 - df$mild_anaemia) * burden_mild[2],
    `EV_lower` = quantile(df$mild_post_2 - df$mild_anaemia, 0.025) * burden_mild[1],
    `EV_upper` = quantile(df$mild_post_2 - df$mild_anaemia, 0.975) * burden_mild[3],
    Model = 1
  )

  diff_moderate <- data.frame(
    Country = country,
    Population = "Moderate anemia",
    Year = pred_year,
    EV = mean(df$moderate_post_2 - df$moderate_anaemia) * burden_moderate[2],
    `EV_lower` = quantile(df$moderate_post_2 - df$moderate_anaemia, 0.025) * burden_moderate[1],
    `EV_upper` = quantile(df$moderate_post_2 - df$moderate_anaemia, 0.975) * burden_moderate[3],
    Model = 1
  )

  diff_severe <- data.frame(
    Country = country,
    Population = "Severe anemia",
    Year = pred_year,
    EV = mean(df$severe_post_2 - df$severe_anaemia) * burden_severe[2],
    EV_lower = quantile(df$severe_post_2 - df$severe_anaemia, 0.025) * burden_severe[1],
    EV_upper = quantile(df$severe_post_2 - df$severe_anaemia, 0.975) * burden_severe[3],
    Model = 1
  )


  df <- rbind(diff_mild, diff_moderate, diff_severe)
  diffset <- rbindlist(list(df, baseline))[, lapply(.SD, sum, na.rm = TRUE), by = list(Country, Population, Year)]


  sim_YLD <- full_join(diffset, baseline) |>
    group_by(Country, Year, Model) |>
    summarise(
      EV = sum(EV),
      EV_lower = sum(EV_lower),
      EV_upper = sum(EV_upper)
    )


  p <- sim_YLD |> ggplot()


  p +
    geom_line(aes(x = Year, y = EV), linewidth = 0.8) +
    geom_ribbon(aes(x = Year, y = EV, ymin = EV_lower, ymax = EV_upper), linetype = "dotted", alpha = 0.1) +
    geom_line(data = sim_YLD[-2,], aes(x = Year, y = EV), linetype = "dashed", linewidth = 0.8) +
    theme_bw()
}




run_sim <- function(country, interventionlist) {
  df <- simulator(
    country = country,
    year = pred_year,
    pop_wra = df_wra,
    pop_anaemic = df_anaemic,
    malaria_weight = 0,
    interventionlist = interventionlist,
    trials = trials
  )
  
  baseline <- subset(
    df_anaemic,
    Country == country &
      Year == pred_year
  ) |>
    select(-c(Pr_pregnant, Pr_pregnant_lower, Pr_pregnant_upper)) |>
    mutate(Model = 0)
  
  diff_mild <- data.frame(
    Country = country,
    Population = "Mild anemia",
    Year = pred_year,
    EV = mean(df$mild_post_2 - df$mild_anaemia),
    `EV_lower` = quantile(df$mild_post_2 - df$mild_anaemia, 0.025),
    `EV_upper` = quantile(df$mild_post_2 - df$mild_anaemia, 0.975),
    Model = 1
  )
  
  diff_moderate <- data.frame(
    Country = country,
    Population = "Moderate anemia",
    Year = pred_year,
    EV = mean(df$moderate_post_2 - df$moderate_anaemia),
    `EV_lower` = quantile(df$moderate_post_2 - df$moderate_anaemia, 0.025),
    `EV_upper` = quantile(df$moderate_post_2 - df$moderate_anaemia, 0.975),
    Model = 1
  )
  
  diff_severe <- data.frame(
    Country = country,
    Population = "Severe anemia",
    Year = pred_year,
    EV = mean(df$severe_post_2 - df$severe_anaemia),
    EV_lower = quantile(df$severe_post_2 - df$severe_anaemia, 0.025),
    EV_upper = quantile(df$severe_post_2 - df$severe_anaemia, 0.975),
    Model = 1
  )
  
  df <- rbind(diff_mild, diff_moderate, diff_severe)
  df <- rbindlist(list(df, baseline))[, lapply(.SD, sum, na.rm = TRUE), by = list(Country, Population, Year)]
  
  diffset <- subset(df_anaemic, Year == 2021 & Country == country) |>
    select(-c(Pr_pregnant, Pr_pregnant_lower, Pr_pregnant_upper))
  
  sim <- full_join(diffset, df)
  
  p <- df_anaemic |>
    filter(Country == country) |>
    ggplot()
  
  
  p +
    geom_line(aes(x = Year, y = EV, colour = Population), linewidth = 0.8) +
    geom_ribbon(aes(x = Year, y = EV, colour = Population, ymin = EV_lower, ymax = EV_upper), linetype = "dotted", alpha = 0.1) +
    geom_line(data = sim, aes(x = Year, y = EV, colour = Population), linetype = "dashed", linewidth = 0.8) +
    theme_bw()
}




# Custom function to plot projected outcomes
make_plot <- function(sim) {
  p <- sim |> ggplot()
  p +
    geom_line(aes(x = Year, y = EV, colour = Population), linewidth = 0.8) +
    geom_ribbon(aes(x = Year, y = EV, colour = Population, ymin = EV_lower, ymax = EV_upper), linetype = "dotted", alpha = 0.1) +
    geom_line(data = sim, aes(x = Year, y = EV, colour = Population), linetype = "dashed", linewidth = 0.8) +
    theme_bw()
}
