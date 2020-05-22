library(shiny)
library(ggplot2)
library(lubridate)
library(stringr)

source("helpers.R")
source("pca_stock_selection.R")

ui <- navbarPage("Index replication",
                 tabPanel("About", 
                          p("This tab will contain method's description")),
                 tabPanel("Simulation",
                          sidebarLayout(
                            sidebarPanel(
                              actionButton("go", "Go"),
                              selectInput("index", h3("Select index"), choices = c("SPX", "DAX")),
                              
                              sliderInput("est_window", h3("Estimation window"), min = 2*252, max= 5*252, value = 2*252),
                              sliderInput("est_frequency", h3("Estimation frequency"), min = 21, max= 12*21, value = 3*21),
                              dateRangeInput("dates", h3("Date range"), start = today() - dyears(15))),
                            mainPanel(plotOutput("simulation"))
                          )
                          )
)


server <- function(input, output){
  
  getData <- reactive({
    data <- query_data(input$index, input$dates[1], input$dates[2])
    data <- process_data(data)
    data
  })
  
  getResults <- reactive({
    results <- run_backtest(data = getData(), stopping_criterion = 0.7, deletion_criterion = 1., 
                            est_window = input$est_window, est_frequency = input$est_frequency)
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





results <- run_backtest(data = data, stopping_criterion = 0.7, deletion_criterion = 1., est_window = 2* 252, est_frequency = 3*21)

results %>% 
  gather("type", "return", -date) %>% 
  ggplot() +
  geom_line(aes(x=as.Date(date), y=return, color=type))


shinyApp(ui, server)