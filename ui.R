library(shiny)
library(lubridate)
library(shinycssloaders)
library(dygraphs)

ui <- navbarPage("Index replication",
                 tabPanel("About", 
                          h2("Introduction"),
                          p("This app allows interactive experimentation with methods from ", 
                            em("'Replicating DAX indicies with Principal Component Analysis'"), 
                            " (2020) M. Andrzejewski, J. Bednarski, J. Cudak"),
                          strong("Key research questions: "),
                          tags$ul(
                            tags$li(strong("How many stocks does it take to create a diversified portfolio?")),
                            tags$ul(tags$li("How many stocks does it take to create a diversified portfolio?")),
                            tags$li(strong("How can we identify which stocks to hold?")),
                            tags$ul(tags$li("Search for risk factors underlying stocks' behaviour"))),
                          h2("PCA stock selection"),
                          p("PCA stock selection procedure is based on method from Yang, Rea and Rea's 2015 study",
                            em("'Stock Selection with Principal Component Analysis'"), " and works as follows:"),
                          tags$ol(
                            tags$li("Apply PCA to the correlation matrix ", withMathJax("\\(\\Sigma\\)"), " of a stock market."),
                            tags$li("Associate one stock with the highest coefficient in absolute value with each
                                     of the last m principal components that have eigenvalues less than a certain
                                     level, d, which is called the ", strong("deletion criterion"), " then delete those m stocks."),
                            tags$li("The procedure is repeated until the eigenvalue of the last principal component 
                                    is not less than level, s, which is called the", strong("stoppping criterion."))
                          ),
                          h3("Lasso stock selection"),
                          p("Lasso stock selection procedure takes advantage of the fact that LASSO regression tends to produce sparse solutions and works as follows:"),
                          tags$ol(
                            tags$li("Estimate \\(\\beta\\) of linear model by minimising: 
                                    $$\\frac{1}{N} \\left\\lVert R_{index} - R \\beta \\right\\rVert^{2}_{2} + \\lambda \\left\\lVert \\beta \\right\\rVert_{1} $$ 
                                    where \\(R_{index}\\) is vector of index's returns, \\(R\\) is the matrix of individual stocks' returns 
                                    and \\(\\lambda\\) is regularization parameter which affects number of selected stocks."),
                            tags$li("Stocks with non-zero coefficients are selected stocks.")
                          )
                          ),
                 tabPanel("Simulation",
                          sidebarLayout(
                            sidebarPanel(
                              includeCSS("styles.css"),
                              div(
                                h4("General settings"),
                                selectInput("index", "Select index", choices = c( "DAX","SPX")),
                                dateRangeInput("dates", "Date range", start = as_date("2020-03-31") - dyears(15), end = as_date("2020-03-31")), 
                                class="general settings"),
                              div(
                                checkboxInput("pca_show", h4("PCA settings"), F),
                                conditionalPanel("input.pca_show",
                                  sliderInput("est_window_pca", "Estimation window", min = 2*252, max= 5*252, value = 2*252),
                                  sliderInput("est_frequency_pca", "Estimation frequency", min = 21, max= 12*21, value = 3*21),
                                  sliderInput("stop_crit", "Stopping Criterion", min=0.5,max=1.5, value=0.7),
                                  sliderInput("del_crit", "Deletion Criterion", min=0.7,max=1.2, value=1.)), 
                                class="pca settings"),
                              div(
                                checkboxInput("lasso_show", h4("Lasso settings"), F),
                                conditionalPanel("input.lasso_show",
                                  sliderInput("est_window_lasso", "Estimation window", min = 2*252, max= 5*252, value = 2*252),
                                  sliderInput("est_frequency_lasso", "Estimation frequency", min = 21, max= 12*21, value = 3*21),
                                  sliderInput("lambda", "Lambda", min=0.0,max=0.99, value=0.99)), 
                                class="lasso settings"),
                              actionButton("go", "Go")),
                            mainPanel(withSpinner(dygraphOutput("cum_return"), type=5),
                                      withSpinner(dygraphOutput("tracking_error"), type=5),
                                      withSpinner(dygraphOutput("n_stocks"), type=5),
                                      withSpinner(tableOutput("stats"), type=5))
                          )
                 )
)
