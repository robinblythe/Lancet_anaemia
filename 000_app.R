library(shiny)
library(shinycssloaders)
library(here)

source(file.path(here(), "constants.R"))

# Define UI
ui <- fluidPage(

  # Application title
  titlePanel("Lancet commission on anaemia"),

  # Select country
  sidebarLayout(
    sidebarPanel(
      selectInput("countries",
        "Select country for analysis",
        choices = c("", sort(country))
      ),
      checkboxGroupInput("interventions",
        "Select interventions to simulate",
        choices = interventionlist
      ),
      actionButton(
        "run",
        "Estimate"
      )
    ),


    # Main panel
    mainPanel(
      plotOutput("sim") |> withSpinner(color = "#AD002AFF")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  output$sim <- renderPlot(NULL)

  # Run simulation based on inputs
  observeEvent(input$run, {
    isolate({
      country <- input$countries
      interventionlist <- input$interventions
    })

    output$sim <- renderPlot(
      run_sim(
        country = country,
        interventionlist = interventionlist
      )
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
