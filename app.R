library(shiny)
library(here)
library(countrycode)

source(file.path(here(), "0_Functions.R"))

# Load country codes
load(file = "countries.Rdata")
load(file = "data.Rdata")

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Lancet commission on anaemia"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("countries",
                  "Select country for analysis",
                  choices = sort(country))
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
