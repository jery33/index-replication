library(DBI)
library(RSQLite)
library(quantmod)
source("helpers.R")


dax_symbols <- paste0(read.csv("data/dax_components.csv")$x, ".DE")
dax_data <- get_symbols(dax_symbols, from="2000-01-01")
dax_data['date'] <- format(dax_data[['date']])
dax_data['index'] <- 'DAX'


spx_symbols <- as.character(read.csv("data/spx_components.csv")$x)
spx_data <- get_symbols(spx_symbols, from="2000-01-01")
spx_data['date'] <- format(spx_data[['date']])
spx_data['index'] <- 'SPX'

data <- bind_rows(list(dax_data, spx_data))
mydb <- dbConnect(RSQLite::SQLite(), "data/prices-db.sqlite")
dbWriteTable(mydb, "stock_prices", data)
dbDisconnect(mydb)
