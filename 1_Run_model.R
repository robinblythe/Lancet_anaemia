# Simulate effectiveness
# Method of moments transformations applied where roughly symmetrical
# Transformation uses https://aushsi.shinyapps.io/ShinyPrior/
# Effectiveness of scaling up iron supplements taken from https://doi.org/10.21203/rs.3.rs-3897976/v1
vd <- rtri(iter, min = 0.21, mode = 0.38, max = 0.69) # voltage drop
effectiveness <- list(
  "Iron_Preg" = 1 - (1 - rbeta(iter, shape1 = 22.598, shape2 = 47.251)) * (1 - vd), # Daily iron in pregnant women
  "Iron_WRA" = 1 - (1 - rbeta(iter, shape1 = 23.118, shape2 = 11.392)) * (1 - vd), # Intermittent iron in WRA
  "Fortification" = rnorm(iter, mean = 0.760, sd = 0.107),
  "Antimalarial" = rbeta(iter, shape1 = 337.799, shape2 = 36.688)
)

# YLDs from anaemia:
# https://doi.org/10.1016/S2352-3026(23)00160-6
YLD_mild <- rbeta(iter, shape1 = 4.007, shape2 = 1091.304)
YLD_moderate <- rbeta(iter, shape1 = 22.992, shape2 = 410.398)
YLD_severe <- rbeta(iter, shape1 = 25.198, 141.630)

# Use a sample from each uncertainty interval for the prevalence data:
df_2030 <- df_prevalence |>
  rowwise() |>
  mutate(
    Pop_anaemic = rtri_catch(
      iter,
      min = Prev_low,
      mode = Prevalence,
      max = Prev_high
    )
  ) |>
  ungroup()

df_2030_by_country <- df_prevalence |>
  select(location_name, Pop_wra, Pr_pregnant_mid, Pr_pregnant_low, Pr_pregnant_high, Pct_malarial) |>
  unique() |>
  rowwise() |>
  mutate(
    Pr_pregnant = rtri_catch(
      iter,
      min = Pr_pregnant_low,
      mode = Pr_pregnant_mid,
      max = Pr_pregnant_high
    )
  ) |>
  ungroup() |>
  mutate(
    Pop_pregnant = Pr_pregnant * Pop_wra,
    Pop_pregnant_malarial = Pop_pregnant * Pct_malarial
  ) |>
  select(location_name, Pr_pregnant, Pop_pregnant, Pop_pregnant_malarial)

df_2030 <- left_join(df_2030, df_2030_by_country, by = join_by(location_name)) |>
  mutate(
    Pop_pregnant_anaemic = Pop_anaemic * Pr_pregnant,
    Pop_pregnant_malaria_anaemic = Pop_pregnant_anaemic * Pct_malarial,
    YLD = case_when(
      rei_name == "Mild anemia" ~ YLD_mild * Pop_anaemic,
      rei_name == "Moderate anemia" ~ YLD_moderate * Pop_anaemic,
      rei_name == "Severe anemia" ~ YLD_severe * Pop_anaemic
    )
  ) |>
  select(
    location_name, rei_name, Pop_wra, Pop_total,
    Pop_anaemic, Pop_pregnant, Pop_pregnant_malarial,
    Pop_pregnant_anaemic, Pop_pregnant_malaria_anaemic, YLD
  )

remove(df_2030_by_country)

# Sample from cost estimates per iteration
df_costs <- df_costs_base |>
  rowwise() |>
  mutate(
    Iron_Preg = rtri(
      iter,
      min = Iron_Preg_Low,
      mode = Iron_Preg_Base,
      max = Iron_Preg_High
    ),
    Iron_WRA = rtri(
      iter,
      min = Iron_WRA_Low,
      mode = Iron_WRA_Base,
      max = Iron_WRA_High
    ),
    Antimalarial = rtri(
      iter,
      min = Antimalarial_Low,
      mode = Antimalarial_Base,
      max = Antimalarial_High
    ),
    Fortification = rtri(
      iter,
      min = Fortification_Low,
      mode = Fortification_Base,
      max = Fortification_High
    )
  ) |>
  select(location_name, Iron_Preg, Iron_WRA, Antimalarial, Fortification)

# Sample from coverage data per iteration using a 25% change
# Need to jointly sample or otherwise current coverage may be larger than maximum possible coverage
perturb <- replicate(8, rnorm(nrow(df_coverage_base), 0, 0.127))
df_coverage <- df_coverage_base[, c(2:9)] + df_coverage_base[, c(2:9)] * perturb
df_coverage[df_coverage > 1] <- 1
df_coverage[df_coverage < 0] <- 0
df_coverage <- df_coverage |>
  mutate(
    Iron_WRA_current = ifelse(Iron_WRA_current > Iron_WRA_max, Iron_WRA_max, Iron_WRA_current),
    Iron_Preg_current = ifelse(Iron_Preg_current > Iron_Preg_max, Iron_Preg_max, Iron_Preg_current),
    Antimalarial_current = ifelse(Antimalarial_current > Antimalarial_max, Antimalarial_max, Antimalarial_current),
    Fortification_current = ifelse(Fortification_current > Fortification_max, Fortification_max, Fortification_current)
  )
df_coverage$Iron_WRA_current <- with(df_coverage, ifelse(Iron_WRA_current > Iron_WRA_max, Iron_WRA_max, Iron_WRA_current))
df_coverage <- cbind(df_coverage_base[1], df_coverage)
remove(perturb)

# Run the simulator function for each intervention:
# Simulator takes prevalence data, country, intervention name, eligible population (for costs)
# and target population (for effects)

# Run analysis for each country
stage0 <- list()
for (i in 1:length(countrylist)) {
  country <- countrylist[i]

  sims <- list(
    # Iron supplementation in pregnant women
    simulator(df_2030, country, interventions[[1]]),
    # Iron supplementation in all WRA
    simulator(df_2030, country, interventions[[2]]),
    # Staple food supplementation for all
    simulator(df_2030, country, interventions[[3]]),
    # Presumptive treatment for malaria in pregnant women
    simulator(df_2030, country, interventions[[4]])
  )

  stage0[[i]] <- do.call(rbind, sims) |>
    group_by(Intervention) |>
    mutate(
      Country = country,
      Cost_per_YLD = ifelse(is.nan(Cost_per_YLD), Inf, Cost_per_YLD)
    ) |>
    relocate(Country, .before = Intervention) |>
    select(-location_name)
}

# Obtain a CEA table with each country and each intervention
cea <- do.call(rbind, stage0) |>
  arrange(Country, Cost_per_YLD) |>
  left_join(WTP,
    by = join_by(Country)
  )
remove(stage0, sims, country, i)

# Identify intervention 1 for each country
int1 <- cea |>
  na.omit() |>
  filter(Cost_per_YLD <= WTP & Cost_per_YLD > 0) |>
  group_by(Country) |>
  slice(1)

# Apply intervention 1 to each country if cost-effective using applicator function
# Applicator function takes the above intervention table and extracts the intervention,
# then applies it to the baseline data provided
df_stage1 <- apply_intervention(base_data = df_2030, cea_table = int1)

# Repeat the analysis, this time excluding the applied intervention
cea0_1 <- cea |>
  group_by(Country) |>
  slice(-1) |>
  select(, 1:2)

# Filter by countries who can cost-effectively apply the intervention
countrylist_2 <- unique(int1$Country)

remove(int1)

# Run simulation for all included countries
stage1 <- list()
for (i in 1:length(countrylist_2)) {
  country <- countrylist_2[i]

  sims <- list(
    # Intervention 1
    simulator(df_stage1, country, cea0_1$Intervention[cea0_1$Country == country][1]),
    # Intervention 2
    simulator(df_stage1, country, cea0_1$Intervention[cea0_1$Country == country][2]),
    # Intervention 3
    simulator(df_stage1, country, cea0_1$Intervention[cea0_1$Country == country][3])
  ) # Add interventions 4 and 5 when intermittent iron is available

  stage1[[i]] <- do.call(rbind, sims) |>
    group_by(Intervention) |>
    mutate(Country = country) |>
    relocate(Country, .before = Intervention) |>
    select(-location_name) |>
    arrange(Cost_per_YLD)
}

# Obtain updated CEA table
cea1 <- do.call(rbind, stage1) |>
  left_join(WTP, by = join_by(Country))
remove(stage1, sims, cea0_1, countrylist_2)



# Identify intervention 2
int2 <- cea1 |>
  na.omit() |>
  filter(Cost_per_YLD <= WTP & Cost_per_YLD > 0) |>
  group_by(Country) |>
  slice(1)

# Apply intervention 2 to each country
df_stage2 <- apply_intervention(base_data = df_stage1, cea_table = int2)


cea1_2 <- cea1 |>
  group_by(Country) |>
  slice(-1) |>
  select(, 1:2)

countrylist_3 <- unique(int2$Country)

remove(cea1, df_stage1, int2)

stage2 <- list()
for (i in 1:length(countrylist_3)) {
  country <- countrylist_3[i]

  sims <- list(
    # Intervention 1
    simulator(df_stage2, country, cea1_2$Intervention[cea1_2$Country == country][1]),
    # Intervention 2
    simulator(df_stage2, country, cea1_2$Intervention[cea1_2$Country == country][2])
  ) # Add interventions 3 and 4 when intermittent iron is available

  stage2[[i]] <- do.call(rbind, sims) |>
    group_by(Intervention) |>
    mutate(Country = country) |>
    relocate(Country, .before = Intervention) |>
    select(-location_name) |>
    arrange(Cost_per_YLD)
}

cea2 <- do.call(rbind, stage2) |>
  left_join(WTP,
    by = join_by(Country)
  )
remove(stage2, sims, cea1_2, countrylist_3)


# Identify intervention 3
int3 <- cea2 |>
  na.omit() |>
  filter(Cost_per_YLD <= WTP & Cost_per_YLD > 0) |>
  group_by(Country) |>
  slice(1)

# Apply intervention 3 to each country
df_stage3 <- apply_intervention(base_data = df_stage2, cea_table = int3)

cea2_3 <- cea2 |>
  group_by(Country) |>
  slice(-1) |>
  select(, 1:2)

countrylist_4 <- unique(int3$Country)
remove(cea2, df_stage2, country, i)

if (identical(countrylist_4, character(0))){
  df_final <- df_stage3
} else {
  
  stage3 <- list()
  for (i in 1:length(countrylist_4)) {
    country <- countrylist_4[i]
    
    stage3[[i]] <- simulator(df_stage3, country, cea2_3$Intervention[cea2_3$Country == country][1]) |>
      group_by(Intervention) |>
      mutate(Country = country) |>
      relocate(Country, .before = Intervention) |>
      select(-location_name) |>
      arrange(Cost_per_YLD)
  }
  
  cea3 <- do.call(rbind, stage3) |>
    left_join(WTP,
              by = join_by(Country)
    )
  remove(stage3, cea2_3, countrylist_4, int3)
  
  
  # Identify intervention 4
  int4 <- cea3 |>
    na.omit() |>
    filter(Cost_per_YLD <= WTP & Cost_per_YLD > 0) |>
    group_by(Country) |>
    slice(1)
  
  # Apply intervention 4 to each country
  # In reality there is no intervention 4; no countries retain cost-effectiveness for the 4th intervention
  df_final <- apply_intervention(base_data = df_stage3, cea_table = int4)
  
  remove(cea3, df_stage3, int3, int4)
  
}

