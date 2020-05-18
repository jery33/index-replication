library(quantmod)
library(tibble)
library(xts)
library(purrr)
library(dplyr)

symbols_str <- "AAPL,MSFT" 
symbols <- unlist(str_split(symbols_str, ", *"))


get_symbol <- function(symbol, ...){
  symbol_xts <- getSymbols(symbol, auto.assign = F, ...)
  adjusted_close <- as.vector(coredata(symbol_xts[,paste(symbol,"Adjusted",sep=".")]))
  tibble(date=index(symbol_xts), price=adjusted_close, symbol=symbol) 
}

get_symbols <- function(symbols, ...){
  symbols %>% 
    map(get_symbol, ...) %>% 
    bind_rows()
}




