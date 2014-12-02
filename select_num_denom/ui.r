

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
        )
      ),
      
      actionButton("action", label = "Get Data"),
      
      uiOutput("numerator"),
      uiOutput("denominator")
      ),
    
    mainPanel(
      
      textOutput("selection"),
      br(),
      textOutput("describe_num_and_denom"),
      h1("table of variables"),
      tableOutput("num_and_dom")
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

