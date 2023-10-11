gc()
options(scipen = 999, digits = 5)

library(tidyverse)
library(countrycode)
library(EnvStats)

Country <- c("Armenia", "Malawi")
pred_year <- 2030

#Obtain population estimates with 2025 forecasts
source("./1_Load_and_wrangle.R")


#Cost per cure by intervention
staple_cost_per <- c(0.10, 0.12, 0.18)
staple_eff_per <- c(0.975, 0.976, 0.978)

#Staple foods, all anaemia types
eff_med <- c()
eff_min <- c()
eff_max <- c()
for (i in 1:length(Country)){

  eff_med <- c(eff_med,
    df_pop$EV[df_pop$Country == Country[i] & df_pop$Year == pred_year]*staple_cost_per[2]/
    ((1-staple_eff_per[2])*sum(df_anaemic$EV[df_anaemic$Country == Country[i] & df_anaemic$Year == pred_year]))
  )

  eff_min <- c(eff_min,
    df_pop$EV_lower[df_pop$Country == Country[i] & df_pop$Year == pred_year]*staple_cost_per[1]/
    ((1-staple_eff_per[1])*sum(df_anaemic$EV_lower[df_anaemic$Country == Country[i] & df_anaemic$Year == pred_year]))
  )
  
  eff_max <- c(eff_max,
    df_pop$EV_upper[df_pop$Country == Country[i] & df_pop$Year == pred_year]*staple_cost_per[3]/
    ((1-staple_eff_per[3])*sum(df_anaemic$EV_lower[df_anaemic$Country == Country[i] & df_anaemic$Year == pred_year]))
  )

}









#Outcomes: at the end of the model, we want prevalence before and after intervention(s)
#Then can multiply prevalence by the YLDs for burden
#look up prevalence of malaria



#Some visualisations:
ggplot(df_anaemic, aes(x = Year, y = EV, colour = Population)) +
  geom_smooth() +
  geom_ribbon(aes(ymin = EV_lower, ymax = EV_upper), linetype = "dotted", alpha = 0.1) +
  facet_wrap(vars(Country), scales = "free_y")


#First need to estimate population for 2025
trials = 100000



#Interventions: for n people, simulate the independent probability that each person:
##Failed intervention 1 & failed intervention 2 = still anaemic
##Did not fail intervention 1 | did not fail intervention 2 = not anaemic
#Staple food fortification: OR 0.976 (CI 0.975 to 0.978)


#Approach 1 - simulate from population distribution, simulate from intervention distribution, combine


#Run n draws where n = prevalence
#Interventions: costs per case, effectiveness (RR of being 'cured')


#Step 2 - Pregnant women in Malaria only: Presumptive anti-malarials
