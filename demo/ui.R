# From https://gist.github.com/ptoche/8461329

# ui.R

shinyUI(
  pageWithSidebar(
    headerPanel(h3("Use of hasClass('shiny-busy') with conditionalPanel - Demo"))
    , 
    sidebarPanel(
      helpText(a("Reference: shiny-discuss/05C_X-XMwyk", href = "https://groups.google.com/forum/#!topic/shiny-discuss/05C_X-XMwyk", target="_blank"))
    )
    , 
    mainPanel(
      h5("The actionButton triggers a time-consuming action:")
      , 
      numericInput(inputId = "delay", label = "Select the delay (in seconds)", value = 2, min = 0), tags$br()
      , 
      actionButton("trigger","actionButton")
      ,
      conditionalPanel(condition = "$('html').hasClass('shiny-busy')"
                       , tags$h5(textOutput("busy"))
      )
      , 
      conditionalPanel(condition = "!$('html').hasClass('shiny-busy')"
                       , tags$h5(textOutput("back"))
      )
    )
  )
)