# Simulate effectiveness
# Method of moments transformations applied where roughly symmetrical
# Using https://aushsi.shinyapps.io/ShinyPrior/
# Effectiveness of scaling up iron supplements taken from https://doi.org/10.21203/rs.3.rs-3897976/v1
vd <- rtri(iter, min = 0.21, mode = 0.38, max = 0.69) #voltage drop
effectiveness <- list(
  Iron_Preg = 1 - (1 - rbeta(iter, shape1 = 11.468, shape2 = 19.786)) * (1 - vd),  # Daily iron in pregnant women
  Iron_WRA = 1 - (1 - rbeta(iter, shape1 = 12.198, shape2 = 16.861)) * (1 - vd), # Daily iron in WRA
  Fortification = rnorm(iter, mean = 0.755, sd = 0.110),
  # IntIron_WRA = rbeta(iter, shape1 = 14.525, shape2 = 6.285),
  Antimalarial = rbeta(iter, shape1 = 337.799, shape2 = 36.688)
)
# effectiveness[["IntIron_Preg"]] <- rnorm(iter, mean = 1.320, sd = 0.245) * effectiveness$DailyIron_Preg

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
    mutate(Country = country) |>
    relocate(Country, .before = Intervention) |>
    select(-location_name)
}

# Obtain a CEA table with each country and each intervention
cea <- do.call(rbind, stage0) |>
  arrange(Country, Cost_per_YLD) |>
  left_join(WTP,
    by = join_by(Country)
  )
remove(stage0, sims)

# Identify intervention 1 for each country
int1 <- cea |>
  na.omit() |>
  filter(Cost_per_YLD <= WTP) |>
  group_by(Country) |>
  slice(1)

# Obtain the estimate of how much this will cost to implement
total_spend <- int1 |>
  select(Country, Cost) |>
  rename(Total_spend_1 = Cost)

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

remove(cea, int1)

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
  filter(Cost_per_YLD <= WTP) |>
  group_by(Country) |>
  slice(1)

total_spend <- int2 |>
  select(Country, Cost) |>
  rename(Total_spend_2 = Cost) |>
  full_join(total_spend,
    by = join_by(Country)
  )


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
  filter(Cost_per_YLD <= WTP) |>
  group_by(Country) |>
  slice(1)

total_spend <- int3 |>
  select(Country, Cost) |>
  rename(Total_spend_3 = Cost) |>
  full_join(total_spend,
    by = join_by(Country)
  )

# Apply intervention 3 to each country
df_stage3 <- apply_intervention(base_data = df_stage2, cea_table = int3)


cea2_3 <- cea2 |>
  group_by(Country) |>
  slice(-1) |>
  select(, 1:2)

countrylist_4 <- unique(int3$Country)

remove(cea2, df_stage2, country, i)

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
remove(stage3, cea2_3, countrylist_4)


# Identify intervention 4
int4 <- cea3 |>
  na.omit() |>
  filter(Cost_per_YLD <= WTP) |>
  group_by(Country) |>
  slice(1)

total_spend <- int4 |>
  select(Country, Cost) |>
  rename(Total_spend_4 = Cost) |>
  full_join(total_spend,
    by = join_by(Country)
  ) |>
  suppressWarnings()

# Apply intervention 4 to each country
# In reality there is no intervention 4; no countries retain cost-effectiveness for the 4th intervention
df_final <- apply_intervention(base_data = df_stage3, cea_table = int4)

remove(cea3, df_stage3, int3, int4)

