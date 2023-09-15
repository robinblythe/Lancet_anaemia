#Create basic model estimating effectiveness of various anaemia treatments
gc()
options(scipen = 999)

library(tidyverse)


#Load in data
import <- "C:/Users/blythe/OneDrive - Queensland University of Technology/WHO anaemia/Population data/"
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
df_wra_prev <- subset(df_wra, metric_name == "Number")





#Approach 1: use the interval estimates and calculate from there
anaemic_mean_global <- sum(df_wra$val[df_wra$measure_name == "Prevalence" & df_wra$metric_name == "Number"])
anaemic_lower_global <- sum(df_wra$lower[df_wra$measure_name == "Prevalence" & df_wra$metric_name == "Number"])
anaemic_upper_global <- sum(df_wra$upper[df_wra$measure_name == "Prevalence" & df_wra$metric_name == "Number"])

#Global population (women of reproductive age)
total_wra_global <- sum((df_wra$val[df_wra$measure_name == "Prevalence" & df_wra$metric_name == "Number"])/
                           (df_wra$val[df_wra$measure_name == "Prevalence" & df_wra$metric_name == "Rate"]))


#Estimated impact of fortification (effectiveness), 100k simulations
#Staple foods, general population: OR 0.976 (CI 0.975 to 0.978) using https://aushsi.shinyapps.io/ShinyPrior/
#Normal distribution probably better here, but can't assume normality (appears right skewed)
n = 100000
s1 = 38197.22
s2 = 918.31
#Cost estimates per individual: 0.06 to 0.15 cents, from https://www.sciencedirect.com/science/article/pii/S0022316623031061
min1 = 0.06
max1 = 0.15

#sample
eff_staple_mean <- rbeta(n, shape1 = s1, shape2 = s2)*anaemic_mean_global
eff_staple_lower <- rbeta(n, shape1 = s1, shape2 = s2)*anaemic_lower_global
eff_staple_upper <- rbeta(n, shape1 = s1, shape2 = s2)*anaemic_upper_global

#Estimated cost of fortification, 100k simulations
cost_staple_mean <- runif(n, min = min1, max = max1)*total_wra_global

#Values:
mean_lower_upper <- c(mean = anaemic_mean_global - mean(eff_staple_mean), #Post-supplementation cases "cured"
                      lower = anaemic_lower_global - quantile(eff_staple_lower, 0.025), #lower interval
                      upper = anaemic_upper_global - quantile(eff_staple_upper, 0.975)) #upper interval
print(format(mean_lower_upper, big.mark = ","))

cost_ests <- c(mean = mean(cost_staple_mean),
               lower = quantile(cost_staple_mean, 0.025),
               upper = quantile(cost_staple_mean, 0.975))
print(format(cost_ests, big.mark = ","))


#Better idea? Use the lower-upper estimates and mean to create a distribution and sample from that instead
#Create distribution for each country for eligible populations
#Sample from that for prevalence (for effectiveness data)
#Divide by beta samples for rate to get total population (for cost data if general population interventions)
#Treat vectors as empirical data

df_total <- df_wra_prev |>
  group_by(location_name) |>
  summarise(val = round(sum(val), digits = 0),
            lower = round(sum(lower), digits = 0),
            upper = round(sum(upper), digits = 0)) |>
  ungroup()

#Note most means are skewed left (mean < median) implying non-Gaussian distribution
#Could try writing a function that takes lower, upper and mean, then creates a distribution based on that?
df_total$median <- (df_total$lower + df_total$upper)/2
df_total$tilt

#Try the triangular distribution - use most likely / lower / upper to obtain dist params
#Aggregate the age groups
#Split by mild/moderate/severe
#Take 100k samples
#Still need to know how much money we need to spend to meet targets
#Could start by ranking interventions by cost-effectiveness
#One malarial, one non-malarial - Malawi, Armenia
#Staple food and pregnant women anti-malarials
#If a value can't be found, pick a flat prior like 0.5
#Health economic way to do it is to start with the most cost effective (cost per cure) and move down the list
