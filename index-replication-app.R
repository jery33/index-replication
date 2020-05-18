library(shiny)
library(ggplot2)
library(lubridate)
library(stringr)

source("helpers.R")


ui <- navbarPage("Index replication",
                 tabPanel("About", 
                          p("This tab will contain method's description")),
                 tabPanel("Simulation",
                          sidebarLayout(
                            sidebarPanel(
                              actionButton("go", "Go"),
                              textInput("symbols", h3("Symbols"), value = "AAPL, MSFT"),
                              dateRangeInput("dates", h3("Date range"), start = today() - dyears(15))),
                            mainPanel(plotOutput("simulation"))
                          )
                          )
)


server <- function(input, output){
  
  dataDownload <- reactive({
    symbols <- unlist(str_split(input$symbols, ", *"))
    get_symbols(symbols, from = input$dates[1], to = input$dates[2])
  })
  
  
  buttonClick <- eventReactive(input$go, {
    dataDownload()
  })
  
  output$simulation <- renderPlot({
    ggplot(buttonClick(), aes(x=date, y=price, color=symbol)) + 
      geom_line()
    })
}



shinyApp(ui, server)