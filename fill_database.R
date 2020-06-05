library(quantmod)
library(rvest)
library(xml2)
library(magrittr)

source("helpers.R")

if(!dir.exists("data")){
  dir.create("data")
}

dax_components_path <- "data/dax_components.csv"

if(!file.exists(dax_components_path)){
  write.csv(dax_components(), dax_components_path)
}

dax_symbols <- paste0(read.csv(dax_components_path)$x, ".DE")
dax_data <- get_symbols(dax_symbols, from="2000-01-01")
dax_data['date'] <- format(dax_data[['date']])
dax_data['index'] <- 'DAX'

spx_components_path <- "data/spx_components.csv"

if(!file.exists(spx_components_path)){
  write.csv(spx_components(), spx_components_path)
}

spx_symbols <- as.character(read.csv(spx_components_path)$x)
spx_data <- get_symbols(spx_symbols, from="2000-01-01")
spx_data['date'] <- format(spx_data[['date']])
spx_data['index'] <- 'SPX'

data <- bind_rows(list(dax_data, spx_data))

write_csv(data, "data/prices.csv")
