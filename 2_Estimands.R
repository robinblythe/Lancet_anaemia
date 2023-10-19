gc()
options(scipen = 999, digits = 5)

library(tidyverse)
library(countrycode)
library(EnvStats)
library(data.table)

countries <- c("Armenia", "Malawi")
pred_year <- 2030

# Obtain population estimates with forecasts of pred_year
source("./1_Load_and_wrangle.R")

#Flag to 
df_anaemic$Model <- 0

#Simulation data
trials <- 100000
# Cost per cure by intervention
staple_cost_per <- rtri(trials, min = 0.10, mode = 0.12, max = 0.18)
staple_eff_per <- rtri(trials, min = 0.975, mode = 0.976, max = 0.978)
staple_coverage <- rtri(trials, min = 0.9, mode = 0.95, max = 0.98)
malarials_cost_per <- rtri(trials, min = 2.70, mode = 3.64, max = 8.20)
malarials_eff_per <- rtri(trials, min = 0.4, mode = 0.6, max = 0.8)
malarials_coverage <- rtri(trials, min = 0.5, mode = 0.6, max = 0.7)
malaria_weights <- c(0, 1)

Armenia <- simulator("Armenia", pred_year, pop_wra = df_wra, pop_anaemic = df_anaemic, malaria_weight = 0)
Malawi <- simulator("Malawi", pred_year, pop_wra = df_wra, pop_anaemic = df_anaemic, malaria_weight = 1)

results <- list()
for (i in 1:length(countries)){
  df <- simulator(countries[i], 
                  year = pred_year, 
                  pop_wra = df_wra, 
                  pop_anaemic = df_anaemic, 
                  malaria_weight = malaria_weights[i])
  
  baseline <- subset(df_anaemic,
                     Country == countries[i] &
                       Year == pred_year) |>
    select(-c(Pr_pregnant, Pr_pregnant_lower, Pr_pregnant_upper))
  
  diff_mild <- data.frame(Country = countries[i],
                 Population = "Mild anemia",
                 Year = pred_year,
                 EV = mean(df$mild_post_supp_chemoprev - df$mild_anaemia),
                 `EV_lower` = quantile(df$mild_post_supp_chemoprev - df$mild_anaemia, 0.025),
                 `EV_upper` = quantile(df$mild_post_supp_chemoprev - df$mild_anaemia, 0.975),
                 Model = 1)
  
  diff_moderate <- data.frame(Country = countries[i],
                     Population = "Moderate anemia",
                     Year = pred_year,
                     EV = mean(df$moderate_post_supp_chemoprev - df$moderate_anaemia),
                     `EV_lower` = quantile(df$moderate_post_supp_chemoprev - df$moderate_anaemia, 0.025),
                     `EV_upper` = quantile(df$moderate_post_supp_chemoprev - df$moderate_anaemia, 0.975),
                     Model = 1)
  
  diff_severe <- data.frame(Country = countries[i],
                   Population = "Severe anemia",
                   Year = pred_year,
                   EV = mean(df$severe_post_supp_chemoprev - df$severe_anaemia),
                   EV_lower = quantile(df$severe_post_supp_chemoprev - df$severe_anaemia, 0.025),
                   EV_upper = quantile(df$severe_post_supp_chemoprev - df$severe_anaemia, 0.975),
                   Model = 1)
  
  df <- rbind(diff_mild, diff_moderate, diff_severe)
  df <- rbindlist(list(df, baseline))[, lapply(.SD, sum, na.rm = TRUE), by = list(Country, Population, Year)]
  
  results[[i]] <- df
  remove(df, baseline, diff_mild, diff_moderate, diff_severe)
}

out <- do.call(rbind, results)



# Graphics: the density plots of number of anaemia cases on x axis, countries as facets, y axis is mild-moderate-severe
p <- df_anaemic |> ggplot()

p +
  geom_line(aes(x = Year, y = EV, colour = Population), linewidth = 0.8) +
  geom_ribbon(aes(x = Year, y = EV, colour = Population, ymin = EV_lower, ymax = EV_upper), linetype = "dotted", alpha = 0.1) +
  geom_line(data = sim, aes(x = Year, y = EV, colour = Population), linetype = "dashed", linewidth = 0.8) +
  facet_wrap(vars(Country), scales = "free_y") +
  theme_bw()

ggsave(filename = "test.jpg", dpi = 320)

write.csv(sim, file = "./post_tx_prev.csv")
write.csv(df_anaemic, file = "./pre_tx_prev.csv")



# Remember to multiply cases by burden at the end?






# Staple foods, all anaemia types
eff_med <- c()
eff_min <- c()
eff_max <- c()
for (i in 1:length(Country)) {
  eff_med <- c(
    eff_med,
    df_pop$EV[df_pop$Country == Country[i] & df_pop$Year == pred_year] * staple_cost_per[2] /
      ((1 - staple_eff_per[2]) * sum(df_anaemic$EV[df_anaemic$Country == Country[i] & df_anaemic$Year == pred_year]))
  )

  eff_min <- c(
    eff_min,
    df_pop$EV_lower[df_pop$Country == Country[i] & df_pop$Year == pred_year] * staple_cost_per[1] /
      ((1 - staple_eff_per[1]) * sum(df_anaemic$EV_lower[df_anaemic$Country == Country[i] & df_anaemic$Year == pred_year]))
  )

  eff_max <- c(
    eff_max,
    df_pop$EV_upper[df_pop$Country == Country[i] & df_pop$Year == pred_year] * staple_cost_per[3] /
      ((1 - staple_eff_per[3]) * sum(df_anaemic$EV_lower[df_anaemic$Country == Country[i] & df_anaemic$Year == pred_year]))
  )
}









# Outcomes: at the end of the model, we want prevalence before and after intervention(s)
# Then can multiply prevalence by the YLDs for burden
# look up prevalence of malaria



# Some visualisations:



# First need to estimate population for 2025



# Interventions: for n people, simulate the independent probability that each person:
## Failed intervention 1 & failed intervention 2 = still anaemic
## Did not fail intervention 1 | did not fail intervention 2 = not anaemic
# Staple food fortification: OR 0.976 (CI 0.975 to 0.978)


# Approach 1 - simulate from population distribution, simulate from intervention distribution, combine


# Run n draws where n = prevalence
# Interventions: costs per case, effectiveness (RR of being 'cured')


# Step 2 - Pregnant women in Malaria only: Presumptive anti-malarials
