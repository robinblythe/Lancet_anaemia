#Create basic model estimating effectiveness of various anaemia treatments
gc()
options(scipen = 5)

library(tidyverse)


#Load in data
import <- "C:/Users/blythe/OneDrive - Queensland University of Technology/WHO anaemia/GBD data/"
df_mild <- read.csv(paste0(import,"mild_anemia_prev_data.csv"))
df_mild$severity <- "mild"
df_moderate <- read.csv(paste0(import,"moderate_anemia_prev_data.csv"))
df_moderate$severity <- "moderate"
df_severe <- read.csv(paste0(import, "severe_anemia_prev_data.csv"))
df_severe$severity <- "severe"

df <- do.call(rbind, list(df_mild, df_moderate, df_severe))
remove(df_mild, df_moderate, df_severe, import)
