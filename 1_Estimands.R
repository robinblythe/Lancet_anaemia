gc()
options(scipen = 999, digits = 5)

library(tidyverse)
library(EnvStats)
library(countrycode)

source("./0_Functions.R")

# Load in data
import <- "C:/Users/blythe/OneDrive - Queensland University of Technology/WHO anaemia/Population data/"
df_mild <- read.csv(paste0(import, "mild_anemia_prev_data.csv"))
df_moderate <- read.csv(paste0(import, "moderate_anemia_prev_data.csv"))
df_severe <- read.csv(paste0(import, "severe_anemia_prev_data.csv"))
df <- do.call(rbind, list(df_mild, df_moderate, df_severe))

fert <- read.csv(paste0(import, "fertility_rates.csv"))

#Standardise country names and use 2021 fertility data
fert$Country.Name <- countryname(fert$Country.Name)
fert <- fert |> select("Country.Name", "X2021") |> na.omit()
colnames(fert) <- c("location_name", "Fertility_Rate_2021")

df$location_name <- countryname(df$location_name)

df <- left_join(df, fert)

remove(df_mild, df_moderate, df_severe, import)


# Anaemia target population: women of reproductive age (15-49)
# Age group IDs: 8 to 14
agegroup <- seq(8, 14, 1)

#Use custom functions with Armenia, Malawi
#To apply to entire dataset, use country = unique(df$location_name)
df_prev <- rbind(obtain_anaemic(data = df, country = c("Armenia", "Malawi"), agegroup = agegroup),
                 obtain_wra(data = df, country = c("Armenia", "Malawi"), agegroup = agegroup),
                 obtain_tot(data = df, country = c("Armenia", "Malawi")))

#Pregnant populations: Fertility rates



#Distribution functions, roughly:
trials = 100000
#min = min_prev, max = max_prev, mode = EV_prev




#Interventions: costs per case, effectiveness (RR of being 'cured')
#1 - All women: Staple food fortification

#2 - Pregnant women only: Presumptive anti-malarials
