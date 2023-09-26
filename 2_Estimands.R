gc()
options(scipen = 999, digits = 5)

library(tidyverse)
library(countrycode)
library(EnvStats)

#Obtain population estimates
source("./1_Load_and_wrangle.R")






#Interventions - multiply whole population by dist of mean and 95% interval - use the standard errors because it's the whole population
#Not simulating individuals, so use population * sample(effectiveness mean, effectiveness SE)

#Distribution functions, roughly:
trials = 100000
#min = min_prev, max = max_prev, mode = EV_prev




#Interventions: costs per case, effectiveness (RR of being 'cured')
#1 - All women: Staple food fortification

#2 - Pregnant women only: Presumptive anti-malarials
