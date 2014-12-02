

shinyUI(fluidPage(
  titlePanel("Test"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "option",
        "Choose something",
        choices=c(
          "Working age population/total population"="working_age_population",
          "Council houses/all houses"="council_houses"
          ),
        )
      ),
    mainPanel(
      textOutput("BlahBlah"),
      br(),
      textOutput("out2")
      )
    )
    
  )
  
)


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

