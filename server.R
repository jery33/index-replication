library(shiny)
library(ggplot2)
library(lubridate)
library(stringr)

source("helpers.R")
source("data_load.R")
source("pca_stock_selection.R")
source("lasso_stock_selection.R")


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
  
  getResults <- reactive({left_join(getPCAResults(), getLassoResults() %>% select(-index), by="date")})
  
  buttonClick <- eventReactive(input$go, {
    getResults()
  })
  
  output$cum_return <- renderPlot({
    buttonClick()  %>% 
      gather("type", "return", pca, lasso, index) %>% 
      ggplot() +
      geom_line(aes(x=as.Date(date), y=return-1, color=type), size = 1) +
      labs(title = paste("Replication of", input$index, "index"),
           subtitle = paste(input$dates[1], "–", input$dates[2]),
           x = "Date",
           y = "Cumulative return") +
      scale_color_manual(labels = c( "Lasso", "PCA", input$index), 
                         breaks = c("lasso", "pca" , "index"),
                         values=setNames(c("#340BDB", "#478F00", "#DB0300"), c("lasso", "pca", "index"))) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      scale_x_date(date_breaks = "years" , date_labels = "%y")
  })
  
  
  output$n_stocks <- renderPlot({
    buttonClick()  %>%
      gather("type", "n_stocks", pca_n_stocks, lasso_n_stocks) %>%
      ggplot() +
      geom_line(aes(x=as.Date(date), y=n_stocks, color=type), size = 1) +
      labs(x = "Date",
           y = "Number of stocks")  +
      scale_color_manual(labels = c( "Lasso", "PCA"),
                         breaks = c("lasso_n_stocks", "pca_n_stocks"),
                         values=setNames(c("#340BDB", "#478F00"), c("lasso_n_stocks", "pca_n_stocks"))) +
      ylim(0, NA) +
      scale_x_date(date_breaks = "years" , date_labels = "%y")
  })
}