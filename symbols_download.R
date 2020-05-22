library(rvest)
library(xml2)
library(magrittr)


spx_components <- function(){
  url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies#S&P_500_component_stocks"
  xpath = '//*[@id="constituents"]'
  html <- read_html(url)
  
  tbl <- html %>% 
    html_nodes(xpath = xpath) %>% 
    .[[1]] %>% 
    html_table(fill=T)
  
  tbl$Symbol
}


dax_components <- function(){
  url <- "https://en.wikipedia.org/wiki/DAX"
  xpath <- '//*[@id="constituents"]'
  
  html <- read_html(url)
  
  tbl <- html %>% 
    html_nodes(xpath = xpath) %>% 
    .[[1]] %>% 
    html_table(fill=T)
  
  tbl$`Ticker symbol`
}




spx <- spx_components()
dax <- dax_components()


write.csv(dax, "data/dax_components.csv")
write.csv(spx, "data/spx_components.csv")
