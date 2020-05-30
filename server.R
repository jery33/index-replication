library(shiny)
library(ggplot2)
library(lubridate)
library(stringr)

source("helpers.R")
source("data_load.R")
source("pca_stock_selection.R")
source("lasso_stock_selection.R")


if(!file.exists("data/prices-db.sqlite")){
  source("fill_database.R")
}


server <- function(input, output, session){
  
  getData <- reactive({
    data <- query_data(input$index, input$dates[1], input$dates[2])
    data <- process_data(data)
    data
  })
  
  getPCAResults <- reactive({
    run_backtest_pca(data = getData(), 
                     stopping_criterion = input$stop_crit, 
                     deletion_criterion = input$del_crit, 
                     est_window = input$est_window_pca, 
                     est_frequency = input$est_frequency_pca)
    
  })
  
  getLassoResults <- reactive({
    run_backtest_lasso(data = getData(), 
                       lambda = input$lambda,
                       est_window = input$est_window_lasso, 
                       est_frequency = input$est_frequency_lasso)
  })
  
  getResults <- reactive({left_join(getPCAResults(), getLassoResults() %>% select(date, lasso), by="date")})
  
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