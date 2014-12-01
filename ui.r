shinyUI(fluidPage(
  titlePanel("D vignettes"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "data_option",
        "Select a vignette",
        choices=c(
          "working_age_population" = "working age population",
          "council_houses" = "council houses"
          ),
        selected=NULL
        ),
      submitButton(text="Start simulation")
      ),
    mainPanel(
      h1(textOutput(
        paste(
          "The vignette: ", data_option  
          )
        )),
      plotOutput(
        "plot"
        )
      )
    )
  
  ))

