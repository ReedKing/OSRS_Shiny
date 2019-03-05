library(shiny)
library(shinydashboard)
library(DT)
library(here)

source(here::here('src', 'helper.R'))
here()

# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = 'OSRS GE Price Forecasts'),
    dashboardSidebar(),
    dashboardBody(
        fluidRow(
            column(width = 8, box(plotlyOutput("forecasted"), width = NULL)),
            column(width = 4, box(selectInput("method", 'Select a Model:', c(
                'Automatic Arima' = 'auto_arima',
                'Exponential Smoothing' = 'exp_smooth'
                ))))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    #output$forecasted <- renderPlotly({    })
}

# Run the application
shinyApp(ui = ui, server = server)
