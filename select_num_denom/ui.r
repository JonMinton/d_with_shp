la_to_dz <- read.csv("data/la_to_dz.csv") 

las <- la_to_dz %>%
  group_by(local_authority) %>%
  summarise %>%
  .$local_authority %>%
  as.vector





shinyUI(fluidPage(
  titlePanel("Test"),
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
        selected=""
      ),
      
      uiOutput("numerator"),
      uiOutput("denominator"),
      actionButton("ok_num_denom", "compile selection"),
      br(),
      br(),
      actionButton("load_shapefile_button", "click to load the shapefile"),
      br(),
      actionButton("make_w_matrix_button", "click to generate the w matrix"),
      br(),
      actionButton("run_model_button", "click to run model"),
      br(),
      sliderInput("posterior_sample_size", "choose posterior sample size",
                  min=1000, max=10000, step=1000, value=1000),
      actionButton("generate_posterior_button", "Click to generate posterior distribution"),
      sliderInput("seg_k", "Choose segregation thresholds",
                  min=0, max=1, value=c(0,1))
      ),
    
    mainPanel(
      textOutput("text01"),
      tableOutput("table01"),
      textOutput("text02"),
      tableOutput("table02"),
      textOutput("text03"),
      plotOutput("plot01"),
      textOutput("text04")
      )
    )
    
))
  

