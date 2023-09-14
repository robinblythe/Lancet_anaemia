#Create basic model estimating effectiveness of various anaemia treatments
gc()
options(scipen = 999)

library(tidyverse)


#Load in data
import <- "C:/Users/blythe/OneDrive - Queensland University of Technology/WHO anaemia/GBD data/"
df_mild <- read.csv(paste0(import,"mild_anemia_prev_data.csv"))
df_moderate <- read.csv(paste0(import,"moderate_anemia_prev_data.csv"))
df_severe <- read.csv(paste0(import, "severe_anemia_prev_data.csv"))

df <- do.call(rbind, list(df_mild, df_moderate, df_severe))
remove(df_mild, df_moderate, df_severe, import)

#Anaemia target population: women of reproductive age (15-49)
#Age group IDs: 8 to 14
agegroup <- seq(8,14,1)
#Subset by female, reproductive age, 2021 only
df_wra <- subset(df, sex == "Female" & age_group_id %in% agegroup & year_id == 2021 & measure == "prevalence")

#Intervention effects, general population
#Staple foods, general population: OR 0.976 (CI 0.975 to 0.978) using https://aushsi.shinyapps.io/ShinyPrior/
rbeta(1, shape1 = 38197.22, shape2 = 918.31)
#Cost estimates per individual: 0.06 to 0.15 cents, from https://www.sciencedirect.com/science/article/pii/S0022316623031061
runif(1, min = 0.06, max = 0.15)

#Global prevalence of anaemia before supplementation
anaemic_mean_global <- sum(df_wra$val[df_wra$measure_name == "Prevalence" & df_wra$metric_name == "Number"])
anaemic_lower_global <- sum(df_wra$lower[df_wra$measure_name == "Prevalence" & df_wra$metric_name == "Number"])
anaemic_upper_global <- sum(df_wra$upper[df_wra$measure_name == "Prevalence" & df_wra$metric_name == "Number"])

#Global population (women of reproductive age)
total_wra_global <- sum((df_wra$val[df_wra$measure_name == "Prevalence" & df_wra$metric_name == "Number"])/
                           (df_wra$val[df_wra$measure_name == "Prevalence" & df_wra$metric_name == "Rate"]))


#Estimated impact of fortification (effectiveness), 100k simulations
eff_staple_mean <- rbeta(100000, shape1 = 38197.22, shape2 = 918.31)*anaemic_mean_global
eff_staple_lower <- rbeta(100000, shape1 = 38197.22, shape2 = 918.31)*anaemic_lower_global
eff_staple_upper <- rbeta(100000, shape1 = 38197.22, shape2 = 918.31)*anaemic_upper_global

#Estimated cost of fortification, 100k simulations
cost_staple_mean <- runif(100000, min = 0.06, max = 0.15)*total_wra_global

#Values:
anaemic_mean_global - mean(eff_staple_mean) #Post-supplementation cases "cured"
anaemic_lower_global - quantile(eff_staple_lower, 0.025) #lower interval
anaemic_upper_global - quantile(eff_staple_upper, 0.975) #upper interval

