library(shiny)
library(ggplot2)
library(lubridate)
library(stringr)

source("helpers.R")
source("pca_stock_selection.R")
source("data_load.R")

ui <- navbarPage("Index replication",
                 tabPanel("About", 
                          p("This tab will contain method's description")),
                 tabPanel("Simulation",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("index", "Select index", choices = c("SPX", "DAX")),
                              dateRangeInput("dates", "Date range", start = today() - dyears(15)),
                              strong("PCA stock selection settings"),
                              sliderInput("est_window", "Estimation window", min = 2*252, max= 5*252, value = 2*252),
                              sliderInput("est_frequency", "Estimation frequency", min = 21, max= 12*21, value = 3*21),
                              sliderInput("stop_crit", "Stopping Criterion", min=0.5,max=1.5, value=0.7),
                              sliderInput("del_crit", "Deletion Criterion", min=0.7,max=1.2, value=1.),
                              actionButton("go", "Go")),
                            mainPanel(plotOutput("simulation"))
                          )
                 )
)


server <- function(input, output, session){
  
  getData <- reactive({
    data <- query_data(input$index, input$dates[1], input$dates[2])
    data <- process_data(data)
    data
  })
  
  getResults <- reactive({
    results <- run_backtest(data = getData(), 
                            stopping_criterion = input$stop_crit, 
                            deletion_criterion = input$del_crit, 
                            est_window = input$est_window, 
                            est_frequency = input$est_frequency)
  })
  
  buttonClick <- eventReactive(input$go, {
    getResults()
  })
  
  output$simulation <- renderPlot({
    buttonClick()  %>% 
      gather("type", "return", -date) %>% 
      ggplot() +
      geom_line(aes(x=as.Date(date), y=return, color=type))
  })
}

shinyApp(ui, server)