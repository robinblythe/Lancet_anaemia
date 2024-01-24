gc()
options(scipen = 100, digits = 5)
library(countrycode)
library(EnvStats)
library(ggplot2)
library(data.table)

# Simulation data
trials <- 100000
pred_year <- 2030
interventionlist <- c("Staple foods supplementation", "Anti-malarial pregnancy chemoprevention")

load(file = "countries.Rdata")
load(file = "data.Rdata")

source("./0_Functions.R")

# Intervention data
staple_eff <- rtri(trials, min = 0.975, mode = 0.976, max = 0.978)
staple_coverage <- rtri(trials, min = 0.9, mode = 0.95, max = 0.98)
malarials_eff <- rtri(trials, min = 0.4, mode = 0.6, max = 0.8)
malarials_coverage <- rtri(trials, min = 0.5, mode = 0.6, max = 0.7)
malaria_weights <- c(0, 1)

# As per GBD study: https://doi.org/10.1186/s13045-021-01202-2
# Disability weight * prevalence = YLD
burden_mild <- rtri(trials, min = 0.001, mode = 0.004, max = 0.008)
burden_moderate <- rtri(trials, min = 0.034, mode = 0.052, max = 0.076)
burden_severe <- rtri(trials, min = 0.101, mode = 0.149, max = 0.209)

# Create year_list to iterate over
year_list <- unique(df_anaemic$Year)

# Create modelled estimates
run_sim_YLD <- function(country, interventionlist) {
  # Run simulator function for each year in dataset
  df <- list()
  for (i in year_list) {
    df[[i]] <- simulator(
      country = country,
      year = i,
      pop_wra = df_wra,
      pop_anaemic = df_anaemic,
      malaria_weight = 0,
      interventionlist = interventionlist,
      trials = trials
    )
    df[[i]]$Year <- i
  }
  # Reduce
  df[sapply(df, is.null)] <- NULL

  # Create stacked df
  sim_YLD <- rbindlist(df)
  remove(df)

  # Plot sim_YLD and sim_prevalence as grid

  p <- sim_YLD |> ggplot()

  p +
    stat_summary(aes(x = Year, y = YLD), fun = median, geom = "path", linewidth = 1) +
    stat_summary(aes(x = Year, y = YLD), 
                 fun.min = function(YLD) quantile(YLD, 0.025), 
                 fun.max = function(YLD) quantile(YLD, 0.975), 
                 geom = "ribbon", alpha = 0.2) +
    stat_summary(data = subset(sim_YLD, Year > 2020) |>
                   mutate(YLD = ifelse(Year == 2021, YLD, YLD_post_2)),
                 aes(x = Year, y = YLD), fun = median, geom = "path", linetype = "dashed", linewidth = 1.2) +
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
