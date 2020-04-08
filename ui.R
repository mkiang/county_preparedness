library(shiny)
library(here)
county_choice <- readRDS(here("data", "county_choice.RDS"))

shinyUI(fluidPage(
    titlePanel(page_title),
    
    ## Top row
    ## How to subset the data ----
    fluidRow(
        column(
            width = 4,
            h4(page_subtitle),
            p(page_desc),
            selectInput(
                inputId = "county_size",
                label = NULL,
                selected = 0,
                choices = list(
                    "Include all counties" = 0,
                    "Exclude counties w/ pop. <1,000" = 1000,
                    "Exclude counties w/ pop. <5,000" = 5000,
                    "Exclude counties w/ pop. <10,000" = 10000,
                    "Exclude counties w/ pop. <25,000" = 25000,
                    "Exclude counties w/ pop. <50,000" = 50000,
                    "Exclude counties w/ pop. <100,000" = 100000
                )
            )
        ),
        column(
            width = 4,
            h4("Risk Factor 1 (x-axis)"),
            risk_factor_selector(value_id = "riskfactor1", 
                                 label = NULL, 
                                 selected_x = "icu_per_capita"),
            sliderInput(inputId = "rf1slider", 
                        label = "Threshold for moderate risk", 
                        min = 0, 
                        max = 150, 
                        step = 1, 
                        value = c(10, 25)),
            selectInput(inputId = "rf1transform", 
                        label = "Axis Transformation",
                        choices = list("None" = "identity",
                                       "Natural log" = "log1p",
                                       "Log10" = "log10"),
                        selected = "log10"),
        ),
        column(
               width = 4, 
            h4("Risk Factor 2 (y-axis)"), 
            risk_factor_selector(value_id = "riskfactor2", 
                                 label = NULL, 
                                 selected_x = "p70older"),
            sliderInput(inputId = "rf2slider", 
                        label = "Threshold for moderate risk", 
                        min = 0, 
                        max = 50, 
                        step = 1, 
                        value = c(10, 15)),
            selectInput(inputId = "rf2transform", 
                        label = "Axis Transformation",
                        choices = list("None" = "identity",
                                       "Natural log" = "log1p",
                                       "Log10" = "log10"),
                        selected = "identity"),
        )
    ),
    hr(),
    
    ## Plotting section ----
    fluidRow(
        column(
            width = 8,
            offset = 0,
            h3("Bivariate Risk Map"),
            align = "center",
            plotOutput("bivariate_map", height = "600px")
        ),
        column(
            width = 4,
            offset = 0,
            h3("Scatterplot"),
            align = "center",
            plotOutput("scatterplot", height = "600px")
        )
    ),
    hr(), 
    # Text section ----
    fluidRow(
        column(
            width = 4,
            h4("Numbers in context"),
            htmlOutput("context_text")
        ),
        column(
            width = 4,
            h4("Highlight a county"),
            selectizeInput(
                inputId = "county",
                label = NULL,
                choices = county_choice,
                selected = 6075,
                multiple = FALSE
            ),
            htmlOutput("county_text")
        ),
        column(
            width = 4,
            more_info
        )
    ),
    hr(),
    # Univariate section ----
    fluidRow(
        column(
            width = 5,
            offset = 1, 
            h4(htmlOutput("risk1_label")),
            plotOutput("risk1_map", height = "300px")
        ),
        column(
            width = 5,
            offset = 1,
            h4(htmlOutput("risk2_label")),
            plotOutput("risk2_map", height = "300px")
        ),
    ),
    fluidRow(
        column(
            width = 5,
            offset = 1, 
            plotOutput("risk1_caterpillar", height = "300px"),
        ),
        column(
            width = 5,
            offset = 1,
            plotOutput("risk2_caterpillar", height = "300px"),
        ),
    ),
    
   
    hr(),

    ## Footer ----
    fluidRow(p(),
             br(),
             column(
                 width = 12,
                 align = 'center',
                 footer_tag,
                 br(),
                 p()
             ))
))
