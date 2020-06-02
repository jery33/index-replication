library(glmnet)


lasso_stock_selection <- function(returns, lambda){
  x <- returns %>% as.matrix()
  y <- rowMeans(returns) %>% as.matrix()
  lasso_fit <- glmnet(x, y, intercept = F)
  
  coefficients <- coef(lasso_fit, s = lambda)
  selected_stocks <- coefficients@Dimnames[[1]][coefficients@i + 1]
  n_stocks <- length(selected_stocks)
  
  list(selected_stocks = selected_stocks, n_stocks = n_stocks)
}


est_end_eval_lasso <- function(data, windows, window_id, lambda){
  est_data <- get_estimation_data(data, windows, window_id)
  test_data <- get_test_data(data, windows, window_id)
  train_result <- lasso_stock_selection(est_data, lambda)
  test_result <- get_test_result(test_data, train_result$selected_stocks)
  test_result["lasso_n_stocks"] <- train_result$n_stocks
  test_result
}  


run_backtest_lasso <- function(data, lambda, est_window, est_frequency){
  windows <- generate_windows(data, est_window, est_frequency)
  n_windows <- length(windows$estimation_start)
  
  1:n_windows %>% 
    map(function(x) est_end_eval_lasso(data, windows, x, lambda)) %>% 
    bind_rows() %>% 
    arrange(date) %>% 
    mutate_at(vars(method_return, index_return), function(x) cumprod(1+x)) %>% 
    set_names(c("date", "lasso", "index", "lasso_n_stocks"))
}
