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
