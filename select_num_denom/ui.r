

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
      
      uiOutput("numerator"),
      uiOutput("denominator"),
      actionButton("ok_num_denom", "compile selection"),
      br(),
      br(),
      actionButton("load_shapefile_button", "click to load the shapefile"),
      br(),
      actionButton("make_w_matrix_button", "click to generated the w matrix"),
      sliderInput("seg_k", "Choose segregation thresholds",
                  min=0, max=1, value=c(0,1))
      
      ),
    
    mainPanel(
      textOutput("text01"),
      tableOutput("table01"),
      textOutput("text02"),
      tableOutput("table02"),
      textOutput("text03")
      )
    )
    
))
  


# shinyUI(fluidPage(
#   titlePanel("D vignettes"),
#   sidebarLayout(
#     sidebarPanel(
#       selectInput(
#         "data_option",
#         "Select a vignette",
#         choices=c(
#           "working_age_population" = "working age population",
#           "council_houses" = "council houses"
#           ),
#         selected=NULL
#         ),
#       submitButton(text="Start simulation")
#       ),
#     mainPanel(
#       h1(textOutput(
#         paste(
#           "The vignette: ", data_option  
#           )
#         )),
#       plotOutput(
#         "plot"
#         )
#       )
#     )
#   
#   ))

