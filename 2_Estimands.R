gc()
options(scipen = 999, digits = 5)

library(tidyverse)
library(countrycode)
library(EnvStats)

#Obtain population estimates
source("./1_Load_and_wrangle.R")

#Cost per cure by intervention
#Sample: Staple foods
eff_armenia_med <- df_pop$EV[df_pop$Country == "Armenia" & df_pop$Year == 2021]*0.12/((1-0.976)*df_anaemic$EV[df_anaemic$Country == "Armenia" & df_anaemic$Year == 2021 & df_anaemic$Population == "Mild anemia"])
eff_armenia_lower <- df_pop$EV_lower[df_pop$Country == "Armenia" & df_pop$Year == 2021]*0.10/((1-0.975)*df_anaemic$EV_lower[df_anaemic$Country == "Armenia" & df_anaemic$Year == 2021 & df_anaemic$Population == "Mild anemia"])
eff_armenia_upper <- df_pop$EV_upper[df_pop$Country == "Armenia" & df_pop$Year == 2021]*0.18/((1-0.978)*df_anaemic$EV_upper[df_anaemic$Country == "Armenia" & df_anaemic$Year == 2021 & df_anaemic$Population == "Mild anemia"])



#Some visualisations:
ggplot(df_anaemic, aes(x = Year, y = EV, colour = Population)) +
  geom_smooth() +
  geom_ribbon(aes(ymin = EV_lower, ymax = EV_upper), linetype = "dotted", alpha = 0.1) +
  facet_wrap(vars(Country), scales = "free_y")

trials = 100000

#Extrapolate to 2025 from 2015
#Use geom_smooth with full range
#Cost per cure or the disability weight as the outcome

#Interventions: for n people, simulate the independent probability that each person:
##Failed intervention 1 & failed intervention 2 = still anaemic
##Did not fail intervention 1 | did not fail intervention 2 = not anaemic
#Staple food fortification: OR 0.976 (CI 0.975 to 0.978)


#Approach 1 - simulate from population distribution, simulate from intervention distribution, combine


#Run n draws where n = prevalence
#Interventions: costs per case, effectiveness (RR of being 'cured')


#Step 2 - Pregnant women in Malaria only: Presumptive anti-malarials
