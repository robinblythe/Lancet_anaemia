#Functions 1, 2, 3: Obtain prevalence estimates for anaemic women of reproductive age, all women of reproductive age, and total population
library(tidyverse)

#Anaemia estimates for women of reproductive age
obtain_anaemic <- function(data, country, agegroup){
  subset(data, 
         sex == "Female" &
           age_group_id %in% agegroup &
           measure == "prevalence" &
           metric_name == "Number" &
           location_name %in% country) |>
  group_by(location_name, rei_name, year_id, metric_name) |>
  summarise(EV_prev = round(sum(val[metric_name == "Number"]), digits = 0),
            min_prev = round(sum(lower[metric_name == "Number"]), digits = 0),
            max_prev = round(sum(upper[metric_name == "Number"]), digits = 0)) |>
  ungroup() |>
  select(-"metric_name")
}

#All women of reproductive age
obtain_wra <- function(data, country, agegroup){
  subset(data,
         sex == "Female" &
           rei_name == "Mild anemia" &
           measure_name == "Prevalence" &
           age_group_id %in% agegroup &
           location_name %in% country) |>
    group_by(location_name, age_group_id, year_id) |>
    summarise(EV_prev = val[metric_name == "Number"]/val[metric_name == "Rate"],
              min_prev = val[metric_name == "Number"]/upper[metric_name == "Rate"],
              max_prev = val[metric_name == "Number"]/lower[metric_name == "Rate"]) |>
    ungroup() |>
    group_by(location_name, year_id) |>
    summarise(EV_prev = sum(EV_prev),
              min_prev = sum(min_prev),
              max_prev = sum(max_prev),
              rei_name = "Women of reproductive age") |>
    relocate(location_name, rei_name)
}


#Total population
obtain_tot <- function(data, country){
  subset(data,
         rei_name == "Mild anemia" &
           measure_name == "Prevalence" &
           location_name %in% country) |>
    group_by(location_name, age_group_id, sex_id, year_id) |>
    summarise(EV_prev = val[metric_name == "Number"]/val[metric_name == "Rate"],
              min_prev = val[metric_name == "Number"]/upper[metric_name == "Rate"],
              max_prev = val[metric_name == "Number"]/lower[metric_name == "Rate"]) |>
    ungroup() |>
    group_by(location_name, year_id) |>
    summarise(EV_prev = sum(EV_prev),
              min_prev = sum(min_prev),
              max_prev = sum(max_prev),
              rei_name = "Total population") |>
    relocate(location_name, rei_name)
}


#Predictor functions
predict.anemia <- function(data, year.start, predict.year, country){
  
  df <- subset(data,
               Year >= year.start)
  
  preds <- data.frame(Country = df$Country,
                      Year = predict.year,
                      Population = df$Population,
                      EV = NA_real_,
                      EV_lower = NA_real_,
                      EV_upper = NA_real_,
                      Pr_pregnant = NA_real_,
                      Pr_pregnant_lower = NA_real_,
                      Pr_pregnant_upper = NA_real_) |>
    distinct()
  
    for (i in 1:length(country)){
      #EV, not pregnant
      model_mild_med <- lm(EV ~ Year, data = subset(df, Country == country[i] & Population == "Mild anemia"), na.action = na.omit)
      model_moderate_med <- lm(EV ~ Year, data = subset(df, Country == country[i] & Population == "Moderate anemia"), na.action = na.omit)
      model_severe_med <- lm(EV ~ Year, data = subset(df, Country == country[i] & Population == "Severe anemia"), na.action = na.omit)
      preds$EV[preds$Country == country[i] & preds$Population == "Mild anemia"] <- max(predict(model_mild_med, newdata = data.frame(Year = predict.year)), 0)
      preds$EV[preds$Country == country[i] & preds$Population == "Moderate anemia"] <- max(predict(model_moderate_med, newdata = data.frame(Year = predict.year)), 0)
      preds$EV[preds$Country == country[i] & preds$Population == "Severe anemia"] <- max(predict(model_severe_med, newdata = data.frame(Year = predict.year)), 0)
      
      #EV lower, not pregnant
      model_mild_min <- lm(EV_lower ~ Year, data = subset(df, Country == country[i] & Population == "Mild anemia"), na.action = na.omit)
      model_moderate_min <- lm(EV_lower ~ Year, data = subset(df, Country == country[i] & Population == "Moderate anemia"), na.action = na.omit)
      model_severe_min <- lm(EV_lower ~ Year, data = subset(df, Country == country[i] & Population == "Severe anemia"), na.action = na.omit)
      preds$EV_lower[preds$Country == country[i] & preds$Population == "Mild anemia"] <- max(predict(model_mild_min, newdata = data.frame(Year = predict.year)), 0)
      preds$EV_lower[preds$Country == country[i] & preds$Population == "Moderate anemia"] <- max(predict(model_moderate_min, newdata = data.frame(Year = predict.year)), 0)
      preds$EV_lower[preds$Country == country[i] & preds$Population == "Severe anemia"] <- max(predict(model_severe_min, newdata = data.frame(Year = predict.year)), 0)
      
      #EV upper, not pregnant
      model_mild_max <- lm(EV_upper ~ Year, data = subset(df, Country == country[i] & Population == "Mild anemia"), na.action = na.omit)
      model_moderate_max <- lm(EV_upper ~ Year, data = subset(df, Country == country[i] & Population == "Moderate anemia"), na.action = na.omit)
      model_severe_max <- lm(EV_upper ~ Year, data = subset(df, Country == country[i] & Population == "Severe anemia"), na.action = na.omit)
      preds$EV_upper[preds$Country == country[i] & preds$Population == "Mild anemia"] <- max(predict(model_mild_max, newdata = data.frame(Year = predict.year)), 0)
      preds$EV_upper[preds$Country == country[i] & preds$Population == "Moderate anemia"] <- max(predict(model_moderate_max, newdata = data.frame(Year = predict.year)), 0)
      preds$EV_upper[preds$Country == country[i] & preds$Population == "Severe anemia"] <- max(predict(model_severe_max, newdata = data.frame(Year = predict.year)), 0)
      
      #Pr, pregnant
      model_mild_med_preg <- lm(Pr_pregnant ~ Year, data = subset(df, Country == country[i] & Population == "Mild anemia"), na.action = na.omit)
      model_moderate_med_preg <- lm(Pr_pregnant ~ Year, data = subset(df, Country == country[i] & Population == "Moderate anemia"), na.action = na.omit)
      model_severe_med_preg <- lm(Pr_pregnant ~ Year, data = subset(df, Country == country[i] & Population == "Severe anemia"), na.action = na.omit)
      preds$Pr_pregnant[preds$Country == country[i] & preds$Population == "Mild anemia"] <- max(predict(model_mild_med_preg, newdata = data.frame(Year = predict.year)), 0)
      preds$Pr_pregnant[preds$Country == country[i] & preds$Population == "Moderate anemia"] <- max(predict(model_moderate_med_preg, newdata = data.frame(Year = predict.year)), 0)
      preds$Pr_pregnant[preds$Country == country[i] & preds$Population == "Severe anemia"] <- max(predict(model_severe_med_preg, newdata = data.frame(Year = predict.year)), 0)
      
      #Pr lower, pregnant
      model_mild_min_preg <- lm(Pr_pregnant_lower ~ Year, data = subset(df, Country == country[i] & Population == "Mild anemia"), na.action = na.omit)
      model_moderate_min_preg <- lm(Pr_pregnant_lower ~ Year, data = subset(df, Country == country[i] & Population == "Moderate anemia"), na.action = na.omit)
      model_severe_min_preg <- lm(Pr_pregnant_lower ~ Year, data = subset(df, Country == country[i] & Population == "Severe anemia"), na.action = na.omit)
      preds$Pr_pregnant_lower[preds$Country == country[i] & preds$Population == "Mild anemia"] <- max(predict(model_mild_min_preg, newdata = data.frame(Year = predict.year)), 0)
      preds$Pr_pregnant_lower[preds$Country == country[i] & preds$Population == "Moderate anemia"] <- max(predict(model_moderate_min_preg, newdata = data.frame(Year = predict.year)), 0)
      preds$Pr_pregnant_lower[preds$Country == country[i] & preds$Population == "Severe anemia"] <- max(predict(model_severe_min_preg, newdata = data.frame(Year = predict.year)), 0)
      
      #Pr upper, pregnant
      model_mild_max_preg <- lm(Pr_pregnant_upper ~ Year, data = subset(df, Country == country[i] & Population == "Mild anemia"), na.action = na.omit)
      model_moderate_max_preg <- lm(Pr_pregnant_upper ~ Year, data = subset(df, Country == country[i] & Population == "Moderate anemia"), na.action = na.omit)
      model_severe_max_preg <- lm(Pr_pregnant_upper ~ Year, data = subset(df, Country == country[i] & Population == "Severe anemia"), na.action = na.omit)
      preds$Pr_pregnant_upper[preds$Country == country[i] & preds$Population == "Mild anemia"] <- max(predict(model_mild_max_preg, newdata = data.frame(Year = predict.year)), 0)
      preds$Pr_pregnant_upper[preds$Country == country[i] & preds$Population == "Moderate anemia"] <- max(predict(model_moderate_max_preg, newdata = data.frame(Year = predict.year)), 0)
      preds$Pr_pregnant_upper[preds$Country == country[i] & preds$Population == "Severe anemia"] <- max(predict(model_severe_max_preg, newdata = data.frame(Year = predict.year)), 0)
      
    }
  preds |> arrange(Country)
}


predict.wra <- function(data, year.start, predict.year, country){
  
  df <- subset(data,
               Country %in% country &
                 Year >= year.start)
  
  preds <- data.frame(Country = country,
                      Year = predict.year,
                      Population = df$Population,
                      EV = NA_real_,
                      EV_lower = NA_real_,
                      EV_upper = NA_real_,
                      Pr_pregnant = NA_real_,
                      Pr_pregnant_lower = NA_real_,
                      Pr_pregnant_upper = NA_real_) |>
    distinct()
  
  for (i in 1:length(country)){
    #EV, not pregnant
    model_med <- lm(EV ~ Year, data = subset(df, Country == country[i]), na.action = na.omit)
    preds$EV[preds$Country == country[i]] <- max(predict(model_med, newdata = data.frame(Year = predict.year)), 0)
    
    #EV lower, not pregnant
    model_min <- lm(EV_lower ~ Year, data = subset(df, Country == country[i]), na.action = na.omit)
    preds$EV_lower[preds$Country == country[i]] <- max(predict(model_min, newdata = data.frame(Year = predict.year)), 0)
    
    #EV upper, not pregnant
    model_max <- lm(EV_upper ~ Year, data = subset(df, Country == country[i]), na.action = na.omit)
    preds$EV_upper[preds$Country == country[i]] <- max(predict(model_max, newdata = data.frame(Year = predict.year)), 0)
    
    #Pr, pregnant
    model_med_preg <- lm(Pr_pregnant ~ Year, data = subset(df, Country == country[i]), na.action = na.omit)
    preds$Pr_pregnant[preds$Country == country[i]] <- max(predict(model_med_preg, newdata = data.frame(Year = predict.year)), 0)
    
    #Pr lower, pregnant
    model_min_preg <- lm(Pr_pregnant_lower ~ Year, data = subset(df, Country == country[i]), na.action = na.omit)
    preds$Pr_pregnant_lower[preds$Country == country[i]] <- max(predict(model_min_preg, newdata = data.frame(Year = predict.year)), 0)
    
    #Pr upper, pregnant
    model_max_preg <- lm(Pr_pregnant_upper ~ Year, data = subset(df, Country == country[i]), na.action = na.omit)
    preds$Pr_pregnant_upper[preds$Country == country[i]] <- max(predict(model_max_preg, newdata = data.frame(Year = predict.year)), 0)
    
  }
  
  preds |> arrange(Country)
}


predict.tot <- function(data, year.start, predict.year, country){
  
  df <- subset(data,
               Country %in% country &
                 Year >= year.start)
  
  preds <- data.frame(Country = country,
                      Year = predict.year,
                      Population = df$Population,
                      EV = NA_real_,
                      EV_lower = NA_real_,
                      EV_upper = NA_real_) |>
    distinct()
  
  for (i in 1:length(country)){
    #EV
    model_med <- lm(EV ~ Year, data = subset(df, Country == country[i]), na.action = na.omit)
    preds$EV[preds$Country == country[i]] <- max(predict(model_med, newdata = data.frame(Year = predict.year)), 0)
    
    #EV lower
    model_min <- lm(EV_lower ~ Year, data = subset(df, Country == country[i]), na.action = na.omit)
    preds$EV_lower[preds$Country == country[i]] <- max(predict(model_min, newdata = data.frame(Year = predict.year)), 0)
    
    #EV upper
    model_max <- lm(EV_upper ~ Year, data = subset(df, Country == country[i]), na.action = na.omit)
    preds$EV_upper[preds$Country == country[i]] <- max(predict(model_max, newdata = data.frame(Year = predict.year)), 0)
  }
  
  preds |> arrange(Country)
}


####################################
#Simulation function: create samples
####################################
simulator <- function(country, year, pop_wra, pop_anaemic, malaria_weight, interventionlist){
  
  pop_wra
  pop_anaemic
  
  staple_coverage <- ifelse("Staple foods supplementation" %in% interventionlist,
                            staple_coverage,
                            0)
  malarials_coverage <- ifelse("Anti-malarial pregnancy chemoprevention" %in% interventionlist,
                               malarials_coverage,
                               0)
  
  df <- data.frame(Pr_pregnant = with(
    subset(pop_wra, Country == country & Year == year),
    rtri(trials,
         min = Pr_pregnant_lower,
         max = Pr_pregnant_upper,
         mode = Pr_pregnant
    )
  ))
  
  df$wra <- with(
    subset(pop_wra, Country == country & Year == year),
    rtri(
      n = trials,
      min = EV_lower,
      max = EV_upper,
      mode = EV
    )
  )
  
  df$pregnant <- df$Pr_pregnant * df$wra
  
  df$mild_anaemia <- with(
    subset(pop_anaemic, Country == country & Year == year & Population == "Mild anemia"),
    rtri(
      n = trials,
      min = EV_lower,
      max = EV_upper,
      mode = EV
    )
  )
  
  df$moderate_anaemia <- with(
    subset(pop_anaemic, Country == country & Year == year & Population == "Moderate anemia"),
    rtri(
      n = trials,
      min = EV_lower,
      max = EV_upper,
      mode = EV
    )
  )
  
  df$severe_anaemia <- with(
    subset(pop_anaemic, Country == country & Year == year & Population == "Severe anemia"),
    rtri(
      n = trials,
      min = EV_lower,
      max = EV_upper,
      mode = EV
    )
  )
  
  df$mild_post_1 <- with(
    df,
    mild_anaemia * (1 - (1 - staple_eff_per) * staple_coverage)
  )
  
  df$moderate_post_1 <- with(
    df,
    moderate_anaemia * (1 - (1 - staple_eff_per) * staple_coverage)
  )
  
  df$severe_post_1 <- with(
    df,
    severe_anaemia * (1 - (1 - staple_eff_per) * staple_coverage)
  )
  
  df$mild_post_2 <- df$mild_post_1 - malaria_weight * (df$Pr_pregnant * df$mild_post_1 * (1 - malarials_eff_per) * malarials_coverage)
  
  df$moderate_post_2 <- df$moderate_post_1 - malaria_weight * (df$Pr_pregnant * df$moderate_post_1 * (1 - malarials_eff_per) * malarials_coverage)
  
  df$severe_post_2 <- df$severe_post_1 - malaria_weight * (df$Pr_pregnant * df$severe_post_1 * (1 - malarials_eff_per) * malarials_coverage)
  
  df
}
