library(glue)
library(dplyr)
library(tidyr)
library(purrr)
library(aws.s3)




read_aws_credentials <- function(){
  con <- file("~/.aws/credentials", "r")
  while(TRUE){
    line <- readLines(con, n=1L)
    if ( length(line) == 0 ) {
      break
    }
    if(grepl("=",line, fixed=T)){
      key_value = unlist(strsplit(line, "="))
      args = list(key_value[2])
      names(args) = toupper(key_value[1])
      do.call(Sys.setenv, args)
    }
  }
  close(con)
}



load_data <- function(){
  local_path <- "data/prices.csv"
  if(file.exists(local_path)){
    read.csv(local_path, stringsAsFactors = F)
  } else{
    read_aws_credentials()
    e <- new.env()
    s3load("prices.csv", bucket = "index-replication", envir=e)
    e$prices
  }
}


query_data <- function(data, index_, from, to){
  data %>% 
    filter(index == index_, date > from, date < to) 
}


process_data <- function(data){
  bad_symbols <- data %>% 
    filter(price <= 0) %>% 
    .$symbol %>% 
    unique()
  
  data %>% 
    filter(!(symbol %in% bad_symbols)) %>% 
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
