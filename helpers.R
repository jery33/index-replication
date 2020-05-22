library(quantmod)
library(tibble)
library(xts)
library(purrr)
library(dplyr)



get_symbol <- function(symbol, ...){
  tryCatch({
    symbol_xts <- getSymbols(symbol, auto.assign = F, ...)
    adjusted_close <- as.vector(coredata(symbol_xts[,paste(symbol,"Adjusted",sep=".")]))
    tibble(date=index(symbol_xts), price=adjusted_close, symbol=symbol) 
  },
  error = function(x) tibble(date=as.Date(numeric()), price=numeric(), symbol=character()) 
  )
}

get_symbols <- function(symbols, ...){
  symbols %>% 
    map(get_symbol, ...) %>% 
    bind_rows()
}





