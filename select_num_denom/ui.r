# Load prerequisites 

require(reshape2)
require(plyr)
require(stringr)
require(ggplot2)
require(maptools)
require(grid)
require(spdep)
require(Rcpp)
require(MASS)
require(CARBayes)
require(shiny)
require(dplyr)

# Load data 

la_to_dz <- read.csv("data/la_to_dz.csv") 

las <- c("all",
         la_to_dz %>%
          group_by(local_authority) %>%
          summarise %>%
          .$local_authority %>%
          as.vector
    )




shinyUI(fluidPage(
  titlePanel("D inference app"),
  sidebarLayout(
    sidebarPanel(
      
      selectInput(
        "option",
        "Choose a dataset",
        choices=c(
          "country of origin, 2001"="coo_2001",
          "religion, 2001"="rel_2001",
          "ethnicity, 2001"="eg_2001",
          "country of origin, 2011"="coo_2011",
          "religion, 2011"="rel_2011",
          "ethnicity, 2011"="eg_2011" 
        ),
        selected=""
      ),
      selectInput(
        "option_la",
        "Choose a Local Authority",
        choices=las,
        selected="all"
      ),
      uiOutput("numerator"),
      uiOutput("denominator"),
      actionButton("ok_num_denom", "compile selection"),
      br(),
      br(),
      actionButton("load_shapefile_button", "click to load the shapefile"),
       br(),
       actionButton("make_w_matrix_button", "click to generate the neighbourhood matrix"),

      br(),br(),
      sliderInput("posterior_sample_size", "choose posterior sample size",
                  min=1000, max=10000, step=1000, value=1000),
      br(),
      actionButton("generate_posterior_button", "click to run model"),
      

       sliderInput("seg_k", "Choose segregation thresholds",
                   min=0, max=1, value=c(0,1))
      ),
    
    mainPanel(
      h1("Checks"),
      p(
        "This secton shows some outputs that allow you to see whether the ",
        "prerequisites required by the model have been loaded successfully"
      ),
      htmlOutput("all_checks"),

      textOutput("report_w_matrix_generated"),
      HTML("<p><b>Third</b>, after selecting the numerators and denominators, combine the data:</p>"),

      tableOutput("show_combined_input_table"),
      tableOutput("report_attributes_linked"),
      br(),
      hr(),
      h1("Analysis"),
      h2("report if posterior generated"),
      textOutput("report_posterior_generated"),
      h2("show plot"),
      plotOutput("show_posterior_distribution"),
      tableOutput("tabulate_posterior")

      )
    )
    
))
  

