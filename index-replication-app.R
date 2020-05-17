library(shiny)
library(ggplot2)



ui <- navbarPage("Index replication",
                 tabPanel("About", 
                          p("This tab will contain method's description")),
                 tabPanel("Simulation",
                          sidebarLayout(
                            sidebarPanel(
                              actionButton("go", "Go"),
                              numericInput("n", "n", 50)),
                            mainPanel(plotOutput("simulation"))
                          )
                          )
)


server <- function(input, output){
  
  data <- eventReactive(input$go, {
    data.frame(x = 1:input$n, y = rnorm(input$n ,0,1))
  })
  
  output$simulation <- renderPlot({ggplot(data(), aes(x=x, y=y)) + geom_point()})
}



shinyApp(ui, server)