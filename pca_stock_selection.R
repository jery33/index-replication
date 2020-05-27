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
