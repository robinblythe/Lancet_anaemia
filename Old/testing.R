# Plan: estimate prevalence of anaemia in 2030
# Expected values based on current trends:
## -> simply take a smoothing spline estimate of anaemia prevalence, out to 2030
## Represents if we just continue as is

# Expected values to use for our analytical method:
## -> Take a smoothed spline estimate of total population at 2030 and multiply by 2021 rate
## Represents if we "freeze" the current level of investment (to use our micronutrient supp method)



# Testing ideas/approaches
gc()
options(scipen = 999)

load("C:/Users/blythe/Documents/R projects/Lancet_anaemia/countries.Rdata")

pred_year <- 2030
source("./1_Load_and_wrangle.R")

df <- df_anaemic |>
  select(Country, Population, Year, EV) |>
  filter(Year < 2030) |>
  pivot_wider(names_from = Population, values_from = EV) |>
  as.data.frame()

test_country <- "Malawi"

newdata <- data.frame(Country = test_country, Year = 2030)
test <- subset(df, Country == test_country)

# playing with ARIMA
library(forecast)

# Testing
# Assumption for constant prevalence - if population grows, coverage (as %age) stays same, then theoretically anemic population == same
# Mild
ggplot(data = test, aes(x = Year, y = `Mild anemia`)) +
  geom_line() +
  geom_segment(aes(x = 2021, y = test$`Mild anemia`[test$Year == 2021], xend = 2030, yend = test$`Mild anemia`[test$Year == 2021]), linetype = "dashed") + # baseline - no change
  geom_point(aes(x = 2030, y = predict(smooth.spline(x = test$Year, y = test$`Mild anemia`), x = 2030)$y), colour = "blue") + # smoother
  geom_point(aes(x = 2030, y = forecast(auto.arima(y = test$`Mild anemia`, xreg = test$Year), xreg = 2030)$mean), colour = "grey") # ARIMA

# Moderate
ggplot(data = test, aes(x = Year, y = `Moderate anemia`)) +
  geom_line() +
  geom_segment(aes(x = 2021, y = test$`Moderate anemia`[test$Year == 2021], xend = 2030, yend = test$`Moderate anemia`[test$Year == 2021]), linetype = "dashed") + # baseline - no change
  geom_point(aes(x = 2030, y = predict(smooth.spline(x = test$Year, y = test$`Moderate anemia`), x = 2030)$y), colour = "blue") + # smoother
  geom_point(aes(x = 2030, y = forecast(auto.arima(y = test$`Moderate anemia`, xreg = test$Year), xreg = 2030)$mean), colour = "grey") # ARIMA

# Severe
ggplot(data = test, aes(x = Year, y = `Severe anemia`)) +
  geom_line() +
  geom_segment(aes(x = 2021, y = test$`Severe anemia`[test$Year == 2021], xend = 2030, yend = test$`Severe anemia`[test$Year == 2021]), linetype = "dashed") + # baseline - no change
  geom_point(aes(x = 2030, y = predict(smooth.spline(x = test$Year, y = test$`Severe anemia`), x = 2030)$y), colour = "blue") + # smoother
  geom_point(aes(x = 2030, y = forecast(auto.arima(y = test$`Severe anemia`, xreg = test$Year), xreg = 2030)$mean), colour = "grey")
