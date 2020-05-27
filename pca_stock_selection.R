library(psych)

cor_kmo <- function(x){
  KMO(cor(x))$MSA
}


pca_stock_selection <- function(returns, stopping_criterion = 0.7, deletion_criterion = 1.){
  
  n_stocks <- ncol(returns)
  
  kmo <- cor_kmo(returns)
  
  while(T){
    corr <- cor(returns)
    # if(any(is.na(corr))){
    #   stop("NA in corr")
    # }
    pca <- eigen(corr)
    isFinished <- all(pca$values > stopping_criterion)
    if(!isFinished){
      mask <- pca$values < deletion_criterion
      to_delete <- unique(apply(abs(pca$vectors[,mask,drop=F]), 2, which.max))
      returns <- returns[,-to_delete,drop=F]
    } else{
      break
    }
  }
  selected_stocks <- colnames(returns)
  
  list(selected_stocks = selected_stocks, kmo = kmo, n_stocks = n_stocks)
}




est_end_eval_pca <- function(data, windows, window_id, stopping_criterion, deletion_criterion){
  est_data <- get_estimation_data(data, windows, window_id)
  test_data <- get_test_data(data, windows, window_id)
  train_result <- pca_stock_selection(est_data,  stopping_criterion, deletion_criterion)
  test_result <- get_test_result(test_data, train_result$selected_stocks)
  test_result
}  


run_backtest_pca <- function(data, stopping_criterion, deletion_criterion, est_window, est_frequency){
  windows <- generate_windows(data, est_window, est_frequency)
  n_windows <- length(windows$estimation_start)
  
  1:n_windows %>% 
    map(function(x) est_end_eval_pca(data, windows, x, stopping_criterion, deletion_criterion)) %>% 
    bind_rows() %>% 
    arrange(date) %>% 
    mutate_at(vars(-date), function(x) cumprod(1+x)) %>% 
    set_names(c("date", "pca", "index"))
}

