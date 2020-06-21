library(shiny)
library(ggplot2)
library(lubridate)
library(stringr)
library(dygraphs)
library(xts)

source("helpers.R")
source("data_load.R")
source("pca_stock_selection.R")
source("lasso_stock_selection.R")

global_data <- load_data()


server <- function(input, output, session){
  
  getData <- reactive({
    data <- query_data(global_data, input$index, input$dates[1], input$dates[2])
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
  

  output$stats <- renderTable({
    
    returns <- buttonClick() %>% 
      select(pca , lasso, index) %>% 
      mutate_all(simple_return) %>% 
      drop_na() 
    
    performance <- returns %>% 
      select(pca, lasso, index) %>% 
      gather(key="type",value="return", pca, lasso, index) %>% 
      group_by(type) %>% 
      summarise(`Return (Annualized)` = mean(return)*252, `Volatility (Annualized)`= sd(return)*252^0.5) %>% 
      select(-type)
    
    tracking <- returns %>% 
      mutate(pca_diff = pca - index, lasso_diff = lasso - index) %>% 
      select(pca_diff, lasso_diff) %>% 
      gather(key="type",value="return", pca_diff, lasso_diff) %>% 
      group_by(type) %>% 
      summarise(`Mean Benchmark Difference  (Annualized)` = mean(return)*252, `Tracking Error (Annualized)`= sd(return)*252^0.5) %>% 
      select(-type) %>% 
      rbind(c(0, 0))
    
    table_results <-  (bind_cols(tibble(Method=c("Lasso","PCA","Index")),performance, tracking))
    table_results
  })
  

  output$cum_return <- renderDygraph({
    
    data2 <- buttonClick() %>% 
      drop_na() %>% 
      select(pca, lasso, index) %>% 
      mutate_all(function(x) x/lag(x)-1)%>% 
      as.data.frame() %>% 
      mutate(pca_error = pca - index, lasso_error = lasso - index) %>% 
      select(pca_error, lasso_error)
    

    data <- buttonClick() %>% 
      drop_na() %>% 
      select(pca, lasso, index) %>% 
      mutate_all(function(x) x-1)%>% 
      as.data.frame()
    
    date <-  buttonClick() %>% 
      drop_na( ) %>% 
      .$date %>% 
      as.Date()
  
    (xts(data, order.by = date)) %>% 
      dygraph(main=paste("Replication of", input$index, "index"),
              group = "dygraphs",
              ylab = "Cumulative return",
              xlab = "Date") %>% 
      dyAxis("y",
             valueFormatter = "function(v){return (v*100).toFixed(1) + '%'}",
             axisLabelFormatter = "function(v){return (v*100).toFixed(0) + '%'}") %>% 
      dySeries("pca", label ="PCA", color="#478F00") %>% 
      dySeries("lasso", label ="Lasso",  color="#340BDB") %>% 
      dySeries("index", label ="Index", color="#DB0300") 
  })


  output$tracking_error <- renderDygraph({
    data2 <- buttonClick() %>% 
      drop_na() %>% 
      select(pca, lasso, index) %>% 
      mutate_all(function(x) x/lag(x)-1)%>% 
      as.data.frame() %>% 
      mutate(pca_error = pca - index, lasso_error = lasso - index) %>% 
      select(pca_error, lasso_error)
    
    date <-  buttonClick() %>% 
      drop_na( ) %>% 
      .$date %>% 
      as.Date()
    
    xts(data2, order.by = date) %>% 
      rollapplyr(width=63, FUN=sd, fill = NA) %>% 
      dygraph(group = "dygraphs",
              ylab = "Tracking error",
              xlab = "Date")
  })


  output$n_stocks <- renderDygraph({
    data <- buttonClick() %>%
      drop_na() %>%
      select(pca_n_stocks, lasso_n_stocks) %>%
      as.data.frame()

    date <-  buttonClick() %>%
      drop_na( ) %>%
      .$date %>%
      as.Date()

    (xts(data, order.by = date)) %>%
      dygraph(ylab = "Number of stocks",
              xlab = "Date",
              group = "dygraphs") %>%
      dySeries("pca_n_stocks", label = "PCA") %>% 
      dySeries("lasso_n_stocks", label ="Lasso") %>% 
      dyOptions(stepPlot = TRUE) %>% 
      dyRangeSelector()
  })

  
}