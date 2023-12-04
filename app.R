library(shiny)
library(here)
library(countrycode)


source(file.path(here(), "0_Functions.R"))

import <- "C:/Users/blythe/OneDrive - Queensland University of Technology/WHO anaemia/Population data/"

# Upload required data
df <- do.call(rbind, list(
  read.csv(paste0(import, "mild_anemia_prev_data.csv")),
  read.csv(paste0(import, "moderate_anemia_prev_data.csv")),
  read.csv(paste0(import, "severe_anemia_prev_data.csv"))
)) |>
  mutate(location_name = countryname(location_name))

fert <- read.csv(paste0(import, "fertility_rates.csv")) |>
  mutate(location_name = countryname(Country.Name)) |>
  select(-c("Country.Name", "Country.Code", "Indicator.Name", "Indicator.Code")) |>
  pivot_longer(!location_name, names_to = "year_id", values_to = "Fertility_Rate") |>
  mutate(year_id = as.integer(gsub("X", "", year_id))) |>
  filter(year_id >= 2000) |>
  suppressWarnings() |>
  na.omit()

still <- read.csv(paste0(import, "Stillbirth-rate-and-deaths_2023.csv")) |>
  mutate(location_name = countryname(Country.Name)) |>
  select("location_name", "Uncertainty.Bounds.", paste0("X", seq(2000, 2021, 1), ".5")) |>
  pivot_longer(!c(location_name, Uncertainty.Bounds.), names_to = "year_id") |>
  na.omit() |>
  pivot_wider(names_from = "Uncertainty.Bounds.", values_from = "value") |>
  mutate(year_id = as.integer(gsub(c("X", ".5"), "", year_id))) |>
  suppressWarnings()


pregnancy <- inner_join(fert, still) |>
  mutate(
    still_mid = Median / 1000 * Fertility_Rate,
    still_low = Lower / 1000 * Fertility_Rate,
    still_high = Upper / 1000 * Fertility_Rate
  ) |>
  mutate(
    Pr_preg_med = ((Fertility_Rate * 0.75) + (still_mid * 0.65)) / 34,
    Pr_preg_low = ((Fertility_Rate * 0.75) + (still_low * 0.55)) / 34,
    Pr_preg_high = ((Fertility_Rate * 0.75) + (still_high * 0.75)) / 34
  ) |>
  select("location_name", "year_id", "Pr_preg_med", "Pr_preg_low", "Pr_preg_high")

remove(fert, still)



# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Lancet commission on anaemia"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      uiOutput("var_ui1")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      print(0)
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # Select country from drop-down menu



  output$var_ui1 <- renderUI({
    selectInput("country_var", "Select country", choices = unique(df$location_name))
  })


  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(x,
      breaks = bins, col = "darkgray", border = "white",
      xlab = "Waiting time to next eruption (in mins)",
      main = "Histogram of waiting times"
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
