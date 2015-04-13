# from: https://gist.github.com/ptoche/8461329 

# server.R

shinyServer(
  function(input, output, session){
    
    # This approach triggers a delay when the box is filled before the actionButton is actioned
    #    busyReactive <- reactive({
    #        delay <- as.integer(input$delay)
    #        if(input$trigger > 0) {Sys.sleep(delay)}
    #        return(input$trigger)
    #    })
    
    observe({
      input$trigger
      if(is.null(input$trigger) || input$trigger == 0){return()}
      isolate(Sys.sleep(input$delay))
    })
    
    
    delayValues <- reactiveValues(total = 0)
    
    observe({
      input$trigger
      if(is.null(input$trigger) || input$trigger == 0){return()} # needed for math below
      delayValues$total <- isolate(delayValues$total) + isolate(input$delay)
    })
    
    output$back <- renderText({
      if(is.null(input$trigger) || input$trigger == 0){return()} 
      paste("Shiny is back!", "The actionButton was activated", input$trigger, "times. You have waited a total of about", delayValues$total, "seconds.", sep = " ")
    })
    
    output$busy <- renderText({
      paste0("Shiny is busy... ", "Expect to wait about ", input$delay, " seconds.")
    })
    
  }
)