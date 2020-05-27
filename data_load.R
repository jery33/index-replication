library(RSQLite)
library(DBI)
library(glue)
library(dplyr)
library(tidyr)
library(purrr)


query_data <- function(index, from, to){
  query <-  glue("SELECT * FROM stock_prices WHERE [index] = '{index}' AND [date] > '{from}' AND [date] < '{to}'")
  mydb <- dbConnect(RSQLite::SQLite(), "data/prices-db.sqlite")
  data <- dbGetQuery(mydb, query)
  dbDisconnect(mydb)
  as_tibble(data)
}


process_data <- function(data){
  data %>% 
    select(date, price, symbol) %>%  
    spread(key=symbol, value=price) %>% 
    fill() %>% 
    mutate_at(vars(-date), simple_return) %>% 
    filter_at(vars(-date), any_vars(!is.na(.)))
}


simple_return <- function(x){
  x/lag(x) - 1
}


generate_windows <- function(data, estimation_window, estimation_frequency){
  estimation_start <- seq(1,nrow(data) - estimation_window-1, estimation_frequency)
  estimation_end <- estimation_start + estimation_window
  test_start <- estimation_end+1
  test_end <- test_start + estimation_frequency
  list(estimation_start=estimation_start,
       estimation_end=estimation_end,
       test_start=test_start,
       test_end=test_end)
}


get_estimation_data <- function(data, windows, window_id){
  start <- windows$estimation_start[window_id]
  end <- windows$estimation_end[window_id]
  data[start:end,] %>% 
    select(-date) %>% 
    select_if(function(x) all(!is.na(x)))
}


get_test_data <- function(data, windows, window_id){
  start <- windows$test_start[window_id]
  end <- windows$test_end[window_id]
  data[start:end,]  
}


get_test_result <- function(test_data, selected_stocks){
  method <- test_data[,c("date", selected_stocks)] %>% 
    gather("symbol", "return", -date) %>% 
    group_by(date) %>% 
    summarise(method_return = mean(return)) 
  
  index <- test_data %>% 
    gather("symbol", "return", -date) %>% 
    group_by(date) %>% 
    summarise(index_return = mean(return, na.rm=T))
  
  left_join(method, index, by = "date")
}


est_end_eval <- function(data, windows, window_id, stopping_criterion, deletion_criterion){
  est_data <- get_estimation_data(data, windows, window_id)
  test_data <- get_test_data(data, windows, window_id)
  train_result <- pca_stock_selection(est_data,  stopping_criterion, deletion_criterion)
  test_result <- get_test_result(test_data, train_result$selected_stocks)
  test_result
}  


run_backtest <- function(data, stopping_criterion, deletion_criterion, est_window, est_frequency){
  windows <- generate_windows(data, est_window, est_frequency)
  n_windows <- length(windows$estimation_start)
  
  1:n_windows %>% 
    map(function(x) est_end_eval(data, windows, x, stopping_criterion, deletion_criterion)) %>% 
    bind_rows() %>% 
    arrange(date) %>% 
    mutate_at(vars(-date), function(x) cumprod(1+x)) %>% 
    set_names(c("date", "method", "index"))
}

