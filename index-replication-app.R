library(shiny)
library(ggplot2)
library(lubridate)
library(stringr)

source("helpers.R")
source("data_load.R")
source("pca_stock_selection.R")
source("lasso_stock_selection.R")

ui <- navbarPage("Index replication",
                 tabPanel("About", 
                          p("This tab will contain method's description"),
                          h1("Introduction"),
                          h2("PCA Stock selection"),
                          withMathJax("\\(\\Sigma\\)"),
                          h3("Lasso Stock selection"),
                          p("\\(\\lambda\\)")),
                 
                 tabPanel("Simulation",
                          sidebarLayout(
                            sidebarPanel(
                              includeCSS("styles.css"),
                              div(
                                h4("General settings"),
                                selectInput("index", "Select index", choices = c("SPX", "DAX")),
                                dateRangeInput("dates", "Date range", start = today() - dyears(15)), 
                              class="general settings"),
                              div(
                                h4("PCA stock selection settings"),
                                sliderInput("est_window_pca", "Estimation window", min = 2*252, max= 5*252, value = 2*252),
                                sliderInput("est_frequency_pca", "Estimation frequency", min = 21, max= 12*21, value = 3*21),
                                sliderInput("stop_crit", "Stopping Criterion", min=0.5,max=1.5, value=0.7),
                                sliderInput("del_crit", "Deletion Criterion", min=0.7,max=1.2, value=1.), 
                              class="pca settings"),
                              div(
                                h4("Lasso stock selection settings"),
                                sliderInput("est_window_lasso", "Estimation window", min = 2*252, max= 5*252, value = 2*252),
                                sliderInput("est_frequency_lasso", "Estimation frequency", min = 21, max= 12*21, value = 3*21),
                                sliderInput("lambda", "Lambda", min=0.001,max=0.01, value=0.005), 
                              class="lasso settings"),
                              actionButton("go", "Go")),
                            mainPanel(plotOutput("simulation"))
                          )
                 )
)


server <- function(input, output, session){
  
  getData <- reactive({
    data <- query_data(input$index, input$dates[1], input$dates[2])
    data <- process_data(data)
    data
  })
  
  getPCAResults <- reactive({
    run_backtest_pca(data = getData(), 
                            stopping_criterion = input$stop_crit, 
                             deletion_criterion = input$del_crit, 
                             est_window = input$est_window_pca, 
                             est_frequency = input$est_frequency_pca)
    
  })
  
  getLassoResults <- reactive({
    run_backtest_lasso(data = getData(), 
                                        lambda = input$lambda,
                                        est_window = input$est_window_lasso, 
                                        est_frequency = input$est_frequency_lasso)
  })
  
  getResults <- reactive({left_join(getPCAResults(), getLassoResults() %>% select(date, lasso), by="date")})
  
  buttonClick <- eventReactive(input$go, {
    getResults()
  })
  
  output$simulation <- renderPlot({
    buttonClick()  %>% 
      gather("type", "return", -date) %>% 
      ggplot() +
      geom_line(aes(x=as.Date(date), y=return, color=type))
  })
}

shinyApp(ui, server)