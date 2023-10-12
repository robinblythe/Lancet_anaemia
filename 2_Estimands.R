gc()
options(scipen = 999, digits = 5)

library(tidyverse)
library(countrycode)
library(EnvStats)

Country <- c("Armenia", "Malawi")
pred_year <- 2030

# Obtain population estimates with 2025 forecasts
source("./1_Load_and_wrangle.R")


trials <- 100000
# Cost per cure by intervention
staple_cost_per <- rtri(trials, min = 0.10, mode = 0.12, max = 0.18)
staple_eff_per <- rtri(trials, min = 0.975, mode = 0.976, max = 0.978)
staple_coverage <- rtri(trials, min = 0.9, mode = 0.95, max = 0.98)
malarials_cost_per <- rtri(trials, min = 2.70, mode = 3.64, max = 8.20)
malarials_eff_per <- rtri(trials, min = 0.4, mode = 0.6, max = 0.8)
malarials_coverage <- rtri(trials, min = 0.5, mode = 0.6, max = 0.7)
malaria_weights <- c(0, 1)


# Sim Malawi
# Obtain WRA estimates
malawi <- data.frame(Pr_pregnant = with(
  subset(df_wra, Country == "Malawi" & Year == 2030),
  rtri(trials,
    min = Pr_pregnant_lower,
    max = Pr_pregnant_upper,
    mode = Pr_pregnant
  )
))

malawi$wra <- with(
  subset(df_wra, Country == "Malawi" & Year == 2030),
  rtri(
    n = trials,
    min = EV_lower,
    max = EV_upper,
    mode = EV
  )
)

malawi$pregnant <- malawi$Pr_pregnant * malawi$wra

malawi$mild_anaemia <- with(
  subset(df_anaemic, Country == "Malawi" & Year == 2030 & Population == "Mild anemia"),
  rtri(
    n = trials,
    min = EV_lower,
    max = EV_upper,
    mode = EV
  )
)

malawi$moderate_anaemia <- with(
  subset(df_anaemic, Country == "Malawi" & Year == 2030 & Population == "Moderate anemia"),
  rtri(
    n = trials,
    min = EV_lower,
    max = EV_upper,
    mode = EV
  )
)

malawi$severe_anaemia <- with(
  subset(df_anaemic, Country == "Malawi" & Year == 2030 & Population == "Severe anemia"),
  rtri(
    n = trials,
    min = EV_lower,
    max = EV_upper,
    mode = EV
  )
)

malawi$mild_anemia_pregnant <- malawi$mild_anaemia * malawi$Pr_pregnant

malawi$moderate_anemia_pregnant <- malawi$moderate_anaemia * malawi$Pr_pregnant

malawi$severe_anemia_pregnant <- malawi$severe_anaemia * malawi$Pr_pregnant

malawi$mild_post_supp <- with(
  malawi,
  mild_anaemia * (1 - (1 - staple_eff_per) * staple_coverage)
)

malawi$moderate_post_supp <- with(
  malawi,
  moderate_anaemia * (1 - (1 - staple_eff_per) * staple_coverage)
)

malawi$severe_post_supp <- with(
  malawi,
  severe_anaemia * (1 - (1 - staple_eff_per) * staple_coverage)
)










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
ggplot(df_anaemic, aes(x = Year, y = EV, colour = Population)) +
  geom_smooth() +
  geom_ribbon(aes(ymin = EV_lower, ymax = EV_upper), linetype = "dotted", alpha = 0.1) +
  facet_wrap(vars(Country), scales = "free_y")


# First need to estimate population for 2025



# Interventions: for n people, simulate the independent probability that each person:
## Failed intervention 1 & failed intervention 2 = still anaemic
## Did not fail intervention 1 | did not fail intervention 2 = not anaemic
# Staple food fortification: OR 0.976 (CI 0.975 to 0.978)


# Approach 1 - simulate from population distribution, simulate from intervention distribution, combine


# Run n draws where n = prevalence
# Interventions: costs per case, effectiveness (RR of being 'cured')


# Step 2 - Pregnant women in Malaria only: Presumptive anti-malarials
